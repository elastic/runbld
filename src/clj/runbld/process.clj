(ns runbld.process
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [cheshire.core :as json]
            [clojure.core.async :as async
             :refer [thread go chan >! <! >!! <!! alts! alts!! close!]]
            [runbld.store :as store]
            [runbld.util.data :as data]
            [runbld.util.date :as date]
            [runbld.util.io :as io])
  (:import (clojure.core.async.impl.channels ManyToManyChannel)
           (clojure.lang Atom Ref)
           (java.io File InputStream)))

(s/defn make-structured-log
  ([line    :- s/Str
    id      :- s/Str
    stream  :- s/Keyword
    metrics :- clojure.lang.Ref]
   {:build-id id
    :stream (name stream)
    :time (date/ms-to-iso)
    :log line
    :size (inc (count (.getBytes line)))
    :ordinal {:total (get-in @metrics [:ordinal :total])
              :stream (get-in @metrics [:ordinal stream])}}))

(s/defn capture-to-es
  ([opts    :- OptsElasticsearch]
   (fn [line]
     (store/save-log! opts (json/decode line)))))

(s/defn inc-ordinals
  [m :- clojure.lang.Ref
   label   :- s/Keyword]
  (alter m update :total (fnil inc 0))
  (alter m update label (fnil inc 0)))

(s/defn inc-bytes
  [m       :- clojure.lang.Ref
   line    :- s/Str
   label   :- s/Keyword]
  (let [line-bytes (inc (count (.getBytes line)))]
    (alter m update :total (fnil + 0) line-bytes)
    (alter m update label (fnil + 0) line-bytes)))

(s/defn start-input-reader! :- ManyToManyChannel
  ([is      :- InputStream
    ch      :- ManyToManyChannel
    label   :- s/Keyword
    ords    :- Ref
    bytes   :- Ref]
   (thread
     (doseq [line (line-seq (io/reader is))]
       (dosync
        (inc-ordinals ords label)
        (inc-bytes bytes line label)
        (>!! ch {:time (date/ms-to-iso)
                 :stream label
                 :line line
                 :ord {:total (@ords :total)
                       :stream (@ords label)}})))
     (keyword (str (name label) "-done")))))

(s/defn start-input-combiner! :- ManyToManyChannel
  ([chs :- [ManyToManyChannel]
    out :- ManyToManyChannel]
   (thread
     (while true
       (let [[log ch] (alts!! chs)]
         (>!! out log))))))

(s/defn add-env! :- nil
  [pbenv newenv]
  (doseq [[k v] newenv]
    (.put pbenv (name k) v)))

(s/defn start :-
  {:start s/Num
   :proc  Process
   :out   ManyToManyChannel
   :bytes Ref
   :threads [ManyToManyChannel]}
  ([pb :- ProcessBuilder]
   (let [start-ms    (System/currentTimeMillis)
         ords        (ref {:total 0 :stderr 0 :stdout 0})
         bytes       (ref {:total 0 :stderr 0 :stdout 0})
         out-ch      (chan)
         err-ch      (chan)
         combined-ch (chan)
         proc        (.start pb)
         stdout (start-input-reader!
                 (.getInputStream proc) out-ch :stdout ords bytes)
         stderr (start-input-reader!
                 (.getErrorStream proc) err-ch :stderr ords bytes)
         combined (start-input-combiner! [out-ch err-ch] combined-ch)]
     {:start start-ms
      :proc proc
      :out combined-ch
      :bytes bytes
      :threads [stdout stderr]})))

(s/defn start-input-multiplexer! :- ManyToManyChannel
  ([in-ch   :- ManyToManyChannel
    out-chs :- [ManyToManyChannel]]
   (go
     (while true
       (let [x (<! in-ch)]
         (doseq [ch out-chs]
           (>! ch x)))))))

(s/defn exec-pb :-
  {:exit-code      s/Num
   :millis-end     s/Num
   :millis-start   s/Num
   :status         s/Str
   :time-end       s/Str
   :time-start     s/Str
   :took           s/Num
   :bytes          Ref}
  ([pb]
   (exec-pb pb []))
  ([pb        :- ProcessBuilder
    listeners :- [ManyToManyChannel]]
   (let [{:keys [start proc out bytes threads]} (start pb)
         multi (start-input-multiplexer! out listeners)
         exit-code (.waitFor proc)
         end (System/currentTimeMillis)
         took (- end start)
         ;; stop reader threads
         output-threads-done (doall (map <!! threads))]
     {:exit-code exit-code
      :millis-end end
      :millis-start start
      :status (if (pos? exit-code) "FAILURE" "SUCCESS")
      :time-end (date/ms-to-iso end)
      :time-start (date/ms-to-iso start)
      :took took
      :bytes bytes})))

(s/defn exec :-
  {:exit-code      s/Num
   :millis-end     s/Num
   :millis-start   s/Num
   :status         s/Str
   :time-end       s/Str
   :time-start     s/Str
   :took           s/Num
   :cmd            [s/Str]
   :cmd-source     s/Str
   :bytes          {:total  s/Num
                    :stdout s/Num
                    :stderr s/Num}}
  ([program args scriptfile]
   (exec program args scriptfile (System/getProperty "user.dir")))
  ([program args scriptfile cwd]
   (exec program args scriptfile cwd {}))
  ([program args scriptfile cwd env]
   (exec program args scriptfile cwd {} {:out [] :err []}))
  ([program args scriptfile cwd env listeners]
   (let [scriptfile* (io/abspath scriptfile)
         dir (io/abspath-file cwd)
         cmd (flatten [program args scriptfile*])
         pb (doto (ProcessBuilder. cmd)
              (.directory dir))
         ;; can only alter the env via this mutable map
         _ (add-env! (.environment pb) env)
         res (exec-pb pb listeners)]
     (flush)
     (merge
      res
      {:cmd cmd
       :cmd-source (slurp scriptfile)
       :bytes @(:bytes res)}))))

(s/defn make-file-listener :- ManyToManyChannel
  ([file]
   (let [ch (chan)]
     (go
       (while true
         (when-let [x (<! ch)]
           (io/spit file (str (json/encode x) "\n") :append true))))
     ch)))

(s/defn exec-with-capture :- ProcessResult
  ([build-id   :- s/Str
    program    :- s/Str
    args       :- [s/Str]
    scriptfile :- s/Str
    cwd        :- s/Str
    outputfile :- s/Str
    es-opts    :- OptsElasticsearch
    env        :- {s/Str s/Str}]
   (let [dir (io/abspath-file cwd)
         outputfile* (io/prepend-path dir outputfile)
         listeners [(make-file-listener outputfile*)]
         result (exec program args scriptfile cwd env listeners)
         out-bytes (get-in result [:bytes :stdout])
         err-bytes (get-in result [:bytes :stderr])
         total-bytes (get-in result [:bytes :total])]
     (merge
      (dissoc result :bytes)
      {:out-bytes out-bytes
       :err-bytes err-bytes
       :total-bytes total-bytes}))))

#_(s/defn exec-with-capture :- ProcessResult
    ([build-id   :- s/Str
      program    :- s/Str
      args       :- [s/Str]
      scriptfile :- s/Str
      cwd        :- s/Str
      stdout     :- s/Str
      stderr     :- s/Str
      structured :- s/Str
      es-opts    :- OptsElasticsearch
      env        :- {s/Str s/Str}]
     (let [dir (io/abspath-file cwd)
           metrics (ref empty-metrics)
           out-file (io/prepend-path dir stdout)
           err-file (io/prepend-path dir stderr)
           struct-file (io/prepend-path dir structured)

           result
           (with-open [outw (java.io.PrintWriter. out-file)
                       errw (java.io.PrintWriter. err-file)
                       structw (java.io.PrintWriter. struct-file)]
             (let [processors
                   {:out [(capture-metrics :stdout metrics)
                          (capture-to-writer *out*)
                          (capture-to-writer outw)
                          (capture-to-writer
                           structw
                           (fn [line]
                             (make-structured-log line build-id
                                                  :stdout metrics)))]
                    :err [(capture-metrics :stderr metrics)
                          (capture-to-writer *err*)
                          (capture-to-writer errw)
                          (capture-to-writer
                           structw
                           (fn [line]
                             (make-structured-log line build-id
                                                  :stderr metrics)))]}]
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
    (-> opts :process :output)
    (-> opts :es)
    {"JAVA_HOME" (-> opts :java :home)})})

(comment
  (let [r (ref {:foo 100})
        f (fn []
            (let [recur? (atom false)]
              (dosync
               (alter r update :foo dec)
               (when (pos? (:foo @r))
                 (prn (.getName (Thread/currentThread)) @r)
                 (reset! recur? true)))
              (Thread/sleep (rand-int 100))
              (when @recur?
                (recur))))]
    (prn [@(future (f))
          @(future (f))
          @(future (f))]))
  )
