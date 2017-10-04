(ns runbld.process
  (:require
   [cheshire.core :as json]
   [clojure.core.async :as async :refer [thread go go-loop chan >! <! >!! <!!
                                         alts! alts!! close!]]
   [runbld.env :as env]
   [runbld.io :as io]
   [runbld.schema :refer :all]
   [runbld.store :as store]
   [runbld.util.data :as data]
   [runbld.util.date :as date]
   [runbld.util.debug :as debug]
   [schema.core :as s])
  (:import
   (clojure.core.async.impl.channels ManyToManyChannel)
   (clojure.lang Atom Ref)
   (java.io File InputStream)
   (java.util UUID)
   (java.util.concurrent TimeUnit)))

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

(s/defn make-structured-log
  ([line    :- s/Str
    label   :- s/Keyword
    ords    :- Ref
    extra   :- {s/Any s/Any}]
   (merge
    {:time (date/ms-to-iso)
     :stream (name label)
     :log line
     :size (inc (count (.getBytes line)))
     :ord {:total (@ords :total)
           :stream (@ords label)}}
    extra)))

(s/defn start-input-reader! :- ManyToManyChannel
  ([is        :- InputStream
    ch        :- ManyToManyChannel
    label     :- s/Keyword
    ords      :- Ref
    bytes     :- Ref
    log-extra :- {s/Any s/Any}]
   (thread
     (doseq [line (line-seq (io/reader is))]
       (let [log (dosync
                  (inc-ordinals ords label)
                  (inc-bytes bytes line label)
                  (make-structured-log line label ords log-extra))]
         (>!! ch log)))
     (close! ch)
     (keyword (str (name label) "-done")))))

(s/defn add-env! :- nil
  [pbenv newenv]
  (.clear pbenv)
  (doseq [[k v] newenv]
    (.put pbenv (name k) v)))

(s/defn start :-
  {:start s/Num
   :proc  Process
   :out   ManyToManyChannel
   :bytes Ref
   :threads [ManyToManyChannel]}
  ([pb :- ProcessBuilder]
   (start pb {}))
  ([pb        :- ProcessBuilder
    log-extra :- {s/Any s/Any}]
   (let [start-ms    (System/currentTimeMillis)
         ords        (ref {:total 0 :stderr 0 :stdout 0})
         bytes       (ref {:total 0 :stderr 0 :stdout 0})
         out-ch      (chan)
         err-ch      (chan)
         proc        (.start pb)
         stdout (start-input-reader!
                 (.getInputStream proc) out-ch :stdout ords bytes log-extra)
         stderr (start-input-reader!
                 (.getErrorStream proc) err-ch :stderr ords bytes log-extra)
         combined-ch (async/merge [out-ch err-ch])]
     {:start start-ms
      :proc proc
      :out combined-ch
      :bytes bytes
      :threads [stdout stderr]})))

(s/defn start-input-multiplexer!
  ([in-ch   :- ManyToManyChannel
    out-chs :- [ManyToManyChannel]]
   (go-loop [x (<! in-ch)]
     (if x
       (do (doseq [ch out-chs]
             (>! ch x))
           (recur (<! in-ch)))
       (doseq [c out-chs]
         (close! c))))))

(s/defn exec-pb :-
  {:exit-code      s/Num
   :millis-end     s/Num
   :millis-start   s/Num
   :status         s/Str
   :time-end       s/Str
   :time-start     s/Str
   :took           s/Num
   :bytes          Ref}
  [pb        :- ProcessBuilder
   listeners :- [ManyToManyChannel]
   log-extra :- {s/Any s/Any}
   timeout   :- (s/maybe s/Str)]
  (let [{:keys [start proc out bytes threads]} (start pb log-extra)
        multi (start-input-multiplexer! out listeners)
        timeout-provided? (not (empty? timeout))
        exit-code (if timeout-provided?
                    (.waitFor proc
                              (date/duration-in-seconds timeout)
                              TimeUnit/SECONDS)
                    (.waitFor proc))
        timed-out? (and (boolean? exit-code)
                        (not exit-code))
        exit-code (if timed-out?
                    (do
                      (.destroyForcibly proc)
                      1)
                    (.exitValue proc))
        end (System/currentTimeMillis)
        took (- end start)]
    {:exit-code exit-code
     :millis-end end
     :millis-start start
     :status (cond
               (and timeout-provided? timed-out?)
               "TIMEOUT"

               (pos? exit-code) "FAILURE"

               :else "SUCCESS")
     :time-end (date/ms-to-iso end)
     :time-start (date/ms-to-iso start)
     :took took
     :bytes bytes}))

(defn exec
  [program args scriptfile cwd
   env listeners log-extra timeout]
  (let [scriptfile* (io/abspath scriptfile)
        dir (io/abspath-file cwd)
        cmd (flatten [program args scriptfile*])
        pb (doto (ProcessBuilder. cmd)
             (.directory dir))
        ;; can only alter the env via this mutable map
        _ (add-env! (.environment pb) env)
        res (exec-pb pb listeners log-extra timeout)]
    (debug/log "Exec:"
               "CWD:" dir
               "CMD:" cmd
               "Script exists?" (.exists (io/file scriptfile*)))
    (flush)
    (merge
     res
     {:cmd cmd
      :cmd-source (slurp scriptfile*)
      :bytes (:bytes res)})))

(s/defn start-file-listener! :- [ManyToManyChannel]
  ([file]
   (start-file-listener! file 100))
  ([file bufsize]
   (let [ch (chan bufsize)]
     [ch (go-loop []
           (when-let [x (<! ch)]
             (io/spit file (str (json/encode x) "\n") :append true)
             (recur)))])))

(s/defn start-es-listener! :- [ManyToManyChannel]
  ([es-opts]
   (store/make-bulk-logger es-opts)))

(s/defn start-writer-listener! :- [ManyToManyChannel]
  ([wtr stream]
   (start-writer-listener! wtr stream 100))
  ([wtr stream bufsize]
   (let [ch (chan bufsize)]
     [ch (go-loop []
           (when-let [x (<! ch)]
             (when (= (name stream) (:stream x))
               (binding [*out* wtr]
                 (println (:log x))))
             (recur)))])))

(def ^:dynamic *process-out* *out*)
(def ^:dynamic *process-err* *err*)

(s/defn exec-with-capture :- ProcessResult
  [program    :- s/Str
   args       :- [s/Str]
   scriptfile :- s/Str
   cwd        :- s/Str
   outputfile :- s/Str
   es-opts    :- OptsElasticsearch
   env        :- Env
   log-extra  :- {s/Any s/Any}
   timeout    :- (s/maybe s/Str)]
  (let [dir (io/abspath-file cwd)
        outputfile* (io/prepend-path dir outputfile)
        [file-ch file-process] (start-file-listener! outputfile*)
        [es-ch es-process] (start-es-listener! es-opts)
        [stdout-ch stdout-process] (start-writer-listener! *process-out*
                                                           :stdout)
        [stderr-ch stderr-process] (start-writer-listener! *process-err*
                                                           :stderr)
        listeners [file-ch es-ch stdout-ch stderr-ch]
        result (exec program args scriptfile cwd
                     env listeners log-extra timeout)
        listeners-done (doall
                        (map <!! [file-process
                                  es-process
                                  stdout-process
                                  stderr-process]))
        out-bytes (@(:bytes result) :stdout)
        err-bytes (@(:bytes result) :stderr)
        total-bytes (@(:bytes result) :total)]
    (store/after-log es-opts)
    (merge
     (dissoc result :bytes)
     {:out-bytes out-bytes
      :err-bytes err-bytes
      :total-bytes total-bytes})))

(def RunOpts
  {:process  OptsProcess
   :es       OptsElasticsearch
   :id       s/Str
   s/Keyword s/Any})

(s/defn run :- (assoc RunOpts :process-result ProcessResult)
  [opts :- RunOpts]
  (assoc opts
         :process-result
         (exec-with-capture
          (-> opts :process :program)
          (-> opts :process :args)
          (-> opts :process :scriptfile)
          (-> opts :process :cwd)
          (-> opts :process :output)
          (-> opts :es)
          (-> opts :process :env)
          {:build-id (-> opts :id)}
          (-> opts :process :timeout))))
