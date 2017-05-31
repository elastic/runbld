(ns runbld.main
  (:gen-class)
  (:require [clj-git.core :as git]
            [clojure.java.io :as jio]
            [clojure.pprint :refer [pprint]]
            [environ.core :as environ]
            [runbld.build :as build]
            [runbld.notifications.email :as email]
            [runbld.notifications.slack :as slack]
            [runbld.java :as java]
            [runbld.opts :as opts]
            [runbld.process :as proc]
            [runbld.scheduler.middleware :as scheduler]
            [runbld.schema :refer :all]
            [runbld.store :as store]
            [runbld.system :as system]
            [runbld.tests :as tests]
            [runbld.io :as io]
            [runbld.pipeline :refer [after around before
                                     debug-log make-pipeline]]
            [runbld.util.date :as date]
            [runbld.vcs.git :refer [checkout-commit]]
            [runbld.vcs.middleware :as vcs]
            [runbld.version :as version]
            [schema.core :as s]
            [slingshot.slingshot :refer [try+ throw+]]))

(defn really-die
  ([code]
   (really-die nil))
  ([code strmsg]
   (when strmsg
     (println strmsg))
   (shutdown-agents)
   ;; Give stdout a chance to finish.  Jenkins can wait.
   (Thread/sleep 5000)
   (System/exit code)))

(defn die
  ([code]
   (die code nil))
  ([code msg]
   (let [msg* (if msg
                (.trim (with-out-str (println msg))))]
     (really-die code msg*)
     ;; for tests when #'really-die is redefed
     msg*)))

(defn wipe-workspace [opts]
  (when (boolean (-> opts :scm :wipe-workspace))
    (let [workspace (System/getenv "WORKSPACE")]
      (io/log "wiping workspace" workspace)
      (io/rmdir-contents workspace)))
  opts)

(s/defn bootstrap-workspace [opts]
  (let [clone? (boolean (-> opts :scm :clone))
        local (-> opts :process :cwd)
        remote (-> opts :scm :url)
        reference (-> opts :scm :reference-repo)
        branch (or (-> opts :scm :branch)
                   (-> opts :build :branch))
        depth (-> opts :scm :depth)]
    (when clone?
      (let [clone-args (->> [(when (and reference
                                        (.exists (jio/as-file reference)))
                               ["--reference" reference])
                             (when branch ["--branch" (str branch)])
                             (when depth ["--depth" (str depth)])]
                            (filter identity)
                            (apply concat))]
        (io/log "cloning" remote)
        (git/git-clone local remote clone-args)
        (io/log "done cloning"))))
  opts)

(defn log-script-execution [proc opts]
  (io/log ">>>>>>>>>>>> SCRIPT EXECUTION BEGIN >>>>>>>>>>>>")
  (let [{:keys [opts process-result] :as res} (proc opts)
        {:keys [took status exit-code out-bytes err-bytes]} process-result]
    (io/log "<<<<<<<<<<<< SCRIPT EXECUTION END <<<<<<<<<<<<")
    (io/log (format "DURATION: %sms" took))
    (io/log (format "STDOUT: %d bytes" out-bytes))
    (io/log (format "STDERR: %d bytes" err-bytes))
    (io/log (format "WRAPPED PROCESS: %s (%d)" status exit-code))
    res))

(defn test-report [opts]
  (assoc opts :test-report (tests/report (-> opts :process :cwd))))

(defn store-result [{:keys [test-report process-result] :as opts}]
  (assoc opts :store-result
         (store/save! opts process-result test-report)))

(defn send-slack [{:keys [store-result] :as opts}]
  (assoc opts :slack-result
         (io/try-log (slack/maybe-send! opts (:addr store-result)))))

(defn send-email [{:keys [store-result] :as opts}]
  (assoc opts :email-result
         (io/try-log (email/maybe-send! opts (:addr store-result)))))

(def default-middleware
  "Middleware that runs during runbld processing. See the docs on
  runbld.pipeline/make-pipeline for more information.

  Order matters as there are stages that may rely on information from
  previous stages.  Pay particular attention to before/after."
  [(before java/add-java)
   (before scheduler/add-scheduler)
   (before build/add-build-meta)
   (before system/add-system-facts)
   (before wipe-workspace)
   (before bootstrap-workspace)
   (before vcs/add-vcs-info)
   (before build/add-last-success)
   (before build/maybe-log-last-success)
   (after  send-slack)
   (after  send-email)
   (after  store-result)
   (after  test-report)
   (around log-script-execution)])

;; -main :: IO ()
(defn -main [& args]
  (try+
   (io/log "runbld started")
   (io/log (version/string))
   (let [raw-opts (assoc (opts/parse-args args) :logger io/log)
         runbld-proc (make-pipeline proc/run default-middleware)
         {:as opts
          :keys [process-result
                 store-result
                 email-result
                 slack-result]} (runbld-proc raw-opts)]
     (if (environ/env :dev)
       (assoc process-result
              :store-result store-result
              :email-result email-result
              :slack-result slack-result)
       (if (-> opts :process :inherit-exit-code)
         (die (-> store-result :build-doc :process :exit-code))
         (die 0))))

   (catch [:error :runbld.main/errors] {:keys [errors msg]}
     (die 3 msg))

   (catch [:error :runbld.opts/parse-error] {:keys [msg]}
     (die 2 msg))

   (catch [:error :runbld.opts/file-not-found] {:keys [msg]}
     (die 2 msg))

   (catch [:help :runbld.opts/version] {:keys [msg]}
     (die 0 msg))

   (catch [:help :runbld.opts/usage] {:keys [msg]}
     (die 0 msg))

   (catch [:help :runbld.opts/system] {:keys [msg]}
     (die 0 (with-out-str
              (clojure.pprint/pprint
               (into (sorted-map)
                     (system/inspect-system "."))))))

   (catch [:error :runbld.vcs.middleware/unknown-repo] {:keys [msg opts]}
     (io/log msg)
     (when (environ/env :dev)
       (io/log "DUMPING CONFIG for DEV=true")
       (io/log
        (with-out-str
          (clojure.pprint/pprint opts))))
     (die 1))

   (catch Exception e
     (die 1 e))))
