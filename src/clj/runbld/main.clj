(ns runbld.main
  (:gen-class)
  (:require [clj-git.core :as git]
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

(defn wipe-workspace [workspace]
  (io/log "wiping workspace" workspace)
  (io/rmdir-contents workspace))

(s/defn bootstrap-workspace
  ([raw-opts :- OptsWithLogger]
   (let [clone? (boolean (-> raw-opts :scm :clone))
         wipe-workspace? (boolean (-> raw-opts :scm :wipe-workspace))
         workspace (System/getenv "WORKSPACE")
         local (-> raw-opts :process :cwd)
         remote (-> raw-opts :scm :url)
         reference (-> raw-opts :scm :reference-repo)
         branch (-> raw-opts :scm :branch)
         depth (-> raw-opts :scm :depth)]
     (when clone?
       (let [clone-args (->> [(when reference ["--reference" reference])
                              (when branch ["--branch" branch])
                              (when depth ["--depth" (str depth)])]
                             (filter identity)
                             (apply concat))]
         (when wipe-workspace?
           (wipe-workspace workspace))
         (io/log "cloning" remote)
         (git/git-clone local remote clone-args)
         (io/log "done cloning"))))))

(def make-opts
  (-> #'identity
      vcs/wrap-vcs-info
      build/wrap-build-meta
      scheduler/wrap-scheduler
      java/wrap-java
      system/wrap-system))

(defn maybe-log-last-success [opts]
  (when (-> opts :build :last-success :checked-out)
    (let [b (build/find-build opts (-> opts :build :last-success :id))
          commit (-> b :vcs :commit-short)]
      (io/log "using last successful commit"
              commit
              "from"
              (-> b :build :job-name) (-> b :id)
              (-> b :process :time-start)
              (date/human-duration
               (date/iso-diff-secs
                (date/from-iso
                 ;; notify on time-end because it makes more
                 ;; logical sense to report on the last
                 ;; completed build's end time, I think
                 (-> b :process :time-end))
                (date/now)))
              "ago"))))

;; -main :: IO ()
(defn -main [& args]
  (try+
   (let [raw-opts (assoc
                   (opts/parse-args args)
                   :logger io/log)
         _ (io/log (version/string))
         _ (bootstrap-workspace raw-opts)
         opts (make-opts raw-opts)
         _ (maybe-log-last-success opts)
         _ (io/log ">>>>>>>>>>>> SCRIPT EXECUTION BEGIN >>>>>>>>>>>>")
         {:keys [opts process-result]} (proc/run opts)
         _ (io/log "<<<<<<<<<<<< SCRIPT EXECUTION END <<<<<<<<<<<<")
         {:keys [took status exit-code out-bytes err-bytes]} process-result
         _ (io/log (format "DURATION: %sms" took))
         _ (io/log (format "STDOUT: %d bytes" out-bytes))
         _ (io/log (format "STDERR: %d bytes" err-bytes))
         _ (io/log (format "WRAPPED PROCESS: %s (%d)" status exit-code))
         test-report (tests/report (-> opts :process :cwd))
         store-result (store/save! opts process-result test-report)
         slack-result (io/try-log (slack/maybe-send! opts (:addr store-result)))
         email-result (io/try-log (email/maybe-send! opts (:addr store-result)))]

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
