(ns runbld.process
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [runbld.store :as store]
            [runbld.util.data :as data]
            [runbld.util.date :as date]
            [runbld.util.io :as io]))

(def empty-metrics
  {:ordinal
   {:global 0
    :stdout 0
    :stderr 0}
   :bytes
   {:global 0
    :stdout 0
    :stderr 0}})

(s/defn capture-metrics
  [stream :- s/Keyword
   metrics :- clojure.lang.Ref]
  (fn [line]
    (let [line-bytes (inc (count (.getBytes line)))]
      (alter metrics update-in [:ordinal :global] inc)
      (alter metrics update-in [:ordinal stream] inc)
      (alter metrics update-in [:bytes :global] + line-bytes)
      (alter metrics update-in [:bytes stream] + line-bytes))))

(s/defn capture-ordinal
  [ord :- clojure.lang.Atom]
  (fn [line]
    (swap! ord inc)))

(s/defn capture-bytes
  [bytes :- clojure.lang.Atom]
  (fn [line]
    ;; inc here to add back the newline stripped off by line-seq
    (swap! bytes + (inc (count (.getBytes line))))))

(defn capture-to-writer [wtr]
  (fn [line]
    (binding [*out* wtr]
      (println line)
      (flush))))

(s/defn capture-to-es
  ([opts    :- OptsElasticsearch
    id      :- s/Str
    stream  :- s/Keyword
    metrics :- clojure.lang.Ref]
   (fn [line]
     (let [doc {:build-id id
                :stream (name stream)
                :time (date/ms-to-iso)
                :log line
                :ordinal {:global (get-in @metrics [:ordinal :global])
                          :stream (get-in @metrics [:ordinal stream])}}]
       (store/save-log! opts doc)))))

(s/defn spit-stream :- s/Keyword
  [input        :- java.io.InputStream
   processors   :- [clojure.lang.IFn]]
  (doseq [line (line-seq (io/reader input))]
    ;; dosync: in order for the global ordinal to order in accordance
    ;; with time, metrics calculation can only happen one stream at a
    ;; time
    (dosync
     (doseq [processor processors]
       (processor line))))
  :done)

(s/defn spit-process :- [java.util.concurrent.Future]
  [stdout :- java.io.InputStream
   stderr :- java.io.InputStream
   processors :- {:out [clojure.lang.IFn]
                  :err [clojure.lang.IFn]}]
  [(future (spit-stream stdout (:out processors)))
   (future (spit-stream stderr (:err processors)))])

(s/defn exec* :-
  {:exit-code      s/Num
   :millis-end     s/Num
   :millis-start   s/Num
   :status         s/Str
   :time-end       s/Str
   :time-start     s/Str
   :took           s/Num}
  ([pb]
   (exec* pb []))
  ([pb :- ProcessBuilder
    output-processors :- {:out [clojure.lang.IFn]
                          :err [clojure.lang.IFn]}]
   (let [start (System/currentTimeMillis)
         proc (.start pb)
         output-futs (spit-process
                      (.getInputStream proc)
                      (.getErrorStream proc)
                      output-processors)
         exit-code (.waitFor proc)
         end (System/currentTimeMillis)
         took (- end start)
         ]
     ;; deref after the timing, to make sure a slow processor doesn't
     ;; skew the process metric
     (doall (map deref output-futs))
     {
      :exit-code exit-code
      :millis-end end
      :millis-start start
      :status (if (pos? exit-code) "FAILURE" "SUCCESS")
      :time-end (date/ms-to-iso end)
      :time-start (date/ms-to-iso start)
      :took took
      })))

(s/defn add-env! :- nil
  [pbenv newenv]
  (doseq [[k v] newenv]
    (.put pbenv (name k) v)))

(s/defn exec :-
  {:exit-code      s/Num
   :millis-end     s/Num
   :millis-start   s/Num
   :status         s/Str
   :time-end       s/Str
   :time-start     s/Str
   :took           s/Num
   :cmd            [s/Str]
   :cmd-source     s/Str}
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

(s/defn exec-with-capture :- ProcessResult
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
         metrics (ref empty-metrics)
         out-file (io/prepend-path dir stdout)
         err-file (io/prepend-path dir stderr)
         result
         (with-open [outw (java.io.PrintWriter. out-file)
                     errw (java.io.PrintWriter. err-file)]
           (let [processors
                 {:out [(capture-metrics :stdout metrics)
                        (capture-to-writer *out*)
                        (capture-to-writer outw)
                        (capture-to-es es-opts
                                       build-id
                                       :stdout
                                       metrics)]
                  :err [(capture-metrics :stderr metrics)
                        (capture-to-writer *err*)
                        (capture-to-writer errw)
                        (capture-to-es es-opts
                                       build-id
                                       :stderr
                                       metrics)]}]
             (exec program args scriptfile cwd env processors)))
         out-bytes (get-in @metrics [:bytes :stdout])
         err-bytes (get-in @metrics [:bytes :stderr])
         out-file-bytes (count (slurp out-file))
         err-file-bytes (count (slurp err-file))]
     (store/after-log es-opts)
     (merge
      result
      {:out-bytes out-bytes
       :err-bytes err-bytes
       :out-file (str out-file)
       :err-file (str err-file)
       :out-file-bytes out-file-bytes
       :err-file-bytes err-file-bytes
       :out-accuracy (data/scaled-percent
                      out-file-bytes out-bytes)
       :err-accuracy (data/scaled-percent
                      err-file-bytes err-bytes)}))))

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
