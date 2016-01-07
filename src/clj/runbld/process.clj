(ns runbld.process
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [runbld.store :as store]
            [runbld.util.data :as data]
            [runbld.util.date :as date]
            [runbld.util.io :as io]))

(defn capture-to-writer [wtr]
  (fn [line]
    (binding [*out* wtr]
      (println line)
      (flush))))

(s/defn capture-to-es
  ([opts    :- OptsElasticsearch
    id      :- s/Str
    type    :- s/Keyword
    ordinal :- clojure.lang.Atom]
   (fn [line]
     (store/save-log! opts id type line @ordinal))))

(s/defn spit-stream :- s/Num
  [input        :- java.io.InputStream
   line-ordinal :- clojure.lang.Atom
   processors   :- [clojure.lang.IFn]]
  (let [bs (atom 0)]
    (doseq [line (line-seq (io/reader input))]
      (doseq [processor processors]
        (processor line))
      (swap! line-ordinal inc)
      (swap! bs + (inc ;; for the newline that gets stripped with
                   ;; line-seq
                   (count (.getBytes line)))))
    @bs))

(defn spit-process [stdout stderr processors]
  [(future
     (spit-stream stdout (:out-ordinal processors) (:out processors)))
   (future
     (spit-stream stderr (:err-ordinal processors) (:err processors)))])

(s/defn process-output :- {:out-bytes java.util.concurrent.Future
                           :err-bytes java.util.concurrent.Future}
  [proc :- Process
   processors :- {:out-ordinal clojure.lang.Atom
                  :err-ordinal clojure.lang.Atom
                  :out [clojure.lang.IFn]
                  :err [clojure.lang.IFn]}]
  (let [[out-bytes err-bytes] (spit-process
                               (.getInputStream proc)
                               (.getErrorStream proc)
                               processors)]
    {:out-bytes out-bytes
     :err-bytes err-bytes}))

(s/defn exec* :- RawProcess
  ([pb]
   (exec* pb []))
  ([pb output-processors]
   (let [start (System/currentTimeMillis)
         proc (.start pb)
         {:keys [out-bytes err-bytes]} (process-output
                                        proc output-processors)
         exit-code (.waitFor proc)
         end (System/currentTimeMillis)
         took (- end start)]
     ;; deref after the timing, to make sure a slow processor doesn't
     ;; skew the process metric
     {
      :err-bytes @err-bytes
      :exit-code exit-code
      :millis-end end
      :millis-start start
      :out-bytes @out-bytes
      :status (if (pos? exit-code) "FAILURE" "SUCCESS")
      :time-end (date/ms-to-iso end)
      :time-start (date/ms-to-iso start)
      :took took
      })))

(s/defn add-env! :- nil
  [pbenv newenv]
  (doseq [[k v] newenv]
    (.put pbenv (name k) v)))

(s/defn exec :- ProcessResultStage1
  ([program args scriptfile]
   (exec program args scriptfile (System/getProperty "user.dir")))
  ([program args scriptfile cwd]
   (exec program args scriptfile cwd {}))
  ([program args scriptfile cwd env]
   (exec program args scriptfile cwd {} {:out [] :err []}))
  ([program args scriptfile cwd env processors]
   (let [scriptfile* (io/abspath scriptfile)
         dir (io/abspath-file cwd)
         cmd (flatten [program args scriptfile*])
         pb (doto (ProcessBuilder. cmd)
              (.directory dir))
         ;; can only alter the env via this mutable map
         _ (add-env! (.environment pb) env)
         res (exec* pb processors)]
     (flush)
     (merge
      res
      {:cmd cmd
       :cmd-source (slurp scriptfile)
       }))))

(s/defn exec-with-capture
  ([build-id   :- s/Str
    program    :- s/Str
    args       :- [s/Str]
    scriptfile :- s/Str
    cwd        :- s/Str
    stdout     :- s/Str
    stderr     :- s/Str
    es-opts    :- OptsElasticsearch
    env        :- {s/Str s/Str}]
   (let [dir (io/abspath-file cwd)
         out-file (io/prepend-path dir stdout)
         out-ordinal (atom 0)
         err-file (io/prepend-path dir stderr)
         err-ordinal (atom 0)
         result
         (with-open [outw (java.io.PrintWriter. out-file)
                     errw (java.io.PrintWriter. err-file)]
           (let [processors
                 {:out-ordinal out-ordinal
                  :err-ordinal err-ordinal
                  :out [(capture-to-writer *out*)
                        (capture-to-writer outw)
                        (capture-to-es es-opts build-id :stdout out-ordinal)]
                  :err [(capture-to-writer *err*)
                        (capture-to-writer errw)
                        (capture-to-es es-opts build-id :stderr err-ordinal)]}]
             (exec program args scriptfile cwd env processors)))
         out-file-bytes (count (slurp out-file))
         err-file-bytes (count (slurp err-file))]
     (store/after-log es-opts)
     (merge
      result
      {:out-file (str out-file)
       :err-file (str err-file)
       :out-file-bytes out-file-bytes
       :err-file-bytes err-file-bytes
       :out-accuracy (data/scaled-percent
                      out-file-bytes (:out-bytes result))
       :err-accuracy (data/scaled-percent
                      err-file-bytes (:err-bytes result))}))))

#_(s/defn output-capturing-run :- ProcessResult
    [opts :- MainOpts]
    (let [dir (io/abspath-file (-> opts :process :cwd))
          out-file (io/prepend-path dir (-> opts :process :stdout))
          out-ordinal (atom 0)
          err-file (io/prepend-path dir (-> opts :process :stderr))
          err-ordinal (atom 0)
          result
          (with-open [outw (java.io.PrintWriter. out-file)
                      errw (java.io.PrintWriter. err-file)]
            (let [processors
                  {:out-ordinal out-ordinal
                   :err-ordinal err-ordinal
                   :out [(capture-to-writer *out*)
                         (capture-to-writer outw)
                         (capture-to-es (:es opts) :stdout out-ordinal)]
                   :err [(capture-to-writer *err*)
                         (capture-to-writer errw)
                         (capture-to-es (:es opts) :stderr err-ordinal)]}]
              (exec
               (-> opts :process :program)
               (-> opts :process :args)
               (-> opts :process :scriptfile)
               (-> opts :process :cwd)
               {"JAVA_HOME" (-> opts :java :home)}
               processors)))
          out-file-bytes (count (slurp out-file))
          err-file-bytes (count (slurp err-file))]
      (merge
       result
       {:out-file (str out-file)
        :err-file (str err-file)
        :out-file-bytes out-file-bytes
        :err-file-bytes err-file-bytes
        :out-accuracy (data/scaled-percent
                       out-file-bytes (:out-bytes result))
        :err-accuracy (data/scaled-percent
                       err-file-bytes (:err-bytes result))})))

(s/defn run :- {(s/required-key :opts) MainOpts
                (s/required-key :process-result) ProcessResult}
  [opts :- MainOpts]
  {:opts opts
   :process-result
   (exec-with-capture
    (-> opts :id)
    (-> opts :process :program)
    (-> opts :process :args)
    (-> opts :process :scriptfile)
    (-> opts :process :cwd)
    (-> opts :process :stdout)
    (-> opts :process :stderr)
    (-> opts :es)
    {"JAVA_HOME" (-> opts :java :home)})})
