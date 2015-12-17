(ns runbld.main
  (:gen-class)
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :as environ]
            [runbld.build :as build]
            [runbld.email :as email]
            [runbld.env :as env]
            [runbld.opts :as opts]
            [runbld.process :as proc]
            [runbld.scheduler.middleware :as scheduler]
            [runbld.store :as store]
            [runbld.system :as system]
            [runbld.tests :as tests]
            [runbld.vcs.middleware :as vcs]
            [schema.core :as s]
            [slingshot.slingshot :refer [try+ throw+]]))

(defn really-die
  ([code]
   (really-die nil))
  ([code strmsg]
   (when strmsg
     (println strmsg))
   (shutdown-agents)
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

(defn log [& s]
  (apply println s))

(def run
  (-> #'proc/run
      vcs/wrap-vcs-info
      build/wrap-build-meta
      scheduler/wrap-scheduler
      env/wrap-env
      system/wrap-system))

;; -main :: IO ()
(defn -main [& args]
  (s/with-fn-validation
    (try+
     (let [opts-init (opts/parse-args args)
           _ (log ">>>>>>>>>>>> SCRIPT EXECUTION BEGIN >>>>>>>>>>>>")
           {:keys [opts process-result]} (run opts-init)
           _ (log "<<<<<<<<<<<< SCRIPT EXECUTION END   <<<<<<<<<<<<")
           {:keys [took status exit-code out-bytes err-bytes]} process-result
           _ (log (format "DURATION: %sms" took))
           _ (log (format "STDOUT: %d bytes" out-bytes))
           _ (log (format "STDERR: %d bytes" err-bytes))
           _ (log (format "WRAPPED PROCESS: %s (%d)" status exit-code))

           test-report (tests/report (-> opts :process :cwd))

           store-result (store/save! opts process-result test-report)
           _ (log (format "SAVED: %s" (:url store-result)))

           email-result (email/maybe-send! opts (:addr store-result))
           _ (log (format "MAILED: %s" (-> email-result :rcpt)))

           ]

       (if (environ/env :dev)
         (assoc process-result
                :store-result store-result
                :email-result email-result)
         (die 0)))

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

     (catch Exception e
       (die 1 e)))))
