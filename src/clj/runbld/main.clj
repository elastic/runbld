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
            [runbld.pipeline :refer [after around before make-pipeline]]
            [runbld.scm :as scm]
            [runbld.util.date :as date]
            [runbld.util.debug :as debug]
            [runbld.vcs.git :refer [checkout-commit]]
            [runbld.vcs.middleware :as vcs]
            [runbld.version :as version]
            [schema.core :as s]
            [slingshot.slingshot :refer [try+]]))

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

(s/defn log-script-execution
  [proc :- clojure.lang.IFn
   opts]
  (io/log ">>>>>>>>>>>> SCRIPT EXECUTION BEGIN >>>>>>>>>>>>")
  (let [{:keys [process-result] :as opts} (proc opts)
        {:keys [took status exit-code out-bytes err-bytes]} process-result]
    (io/log "<<<<<<<<<<<< SCRIPT EXECUTION END <<<<<<<<<<<<")
    (io/log (format "DURATION: %sms" took))
    (io/log (format "STDOUT: %d bytes" out-bytes))
    (io/log (format "STDERR: %d bytes" err-bytes))
    (io/log (format "WRAPPED PROCESS: %s (%d)" status exit-code))
    opts))

(def default-middleware
  "Middleware that runs during runbld processing. See the docs on
  runbld.pipeline/make-pipeline for more information.

  Order matters as there are stages that may rely on information from
  previous stages.  Pay particular attention to before/after."
  [(around debug/with-debug-logging)
   (before java/add-java)
   (before scheduler/add-scheduler)
   (before build/add-build-meta)
   (before store/store-result) ;; store that we started
   (before scm/wipe-workspace)
   (before scm/bootstrap-workspace)
   (before system/add-system-facts)
   (before vcs/add-vcs-info)
   (before build/add-last-success)
   (before build/maybe-log-last-success)
   (after  slack/send-slack)
   (after  email/send-email)
   (after  store/store-result) ;; store that we finished
   (after  tests/add-test-report)
   (around log-script-execution)])

;; -main :: IO ()
(defn -main [& args]
  (try+
   (io/log "runbld started")
   (io/log (version/string))
   (debug/log (version/string))
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

   ;; Gotta catch 'em all!
   (catch Throwable t
     (die 1 t))))
