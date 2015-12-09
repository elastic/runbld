(ns runbld.main
  (:gen-class)
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :as environ]
            [runbld.build :as build]
            [runbld.env :as env]
            [runbld.opts :as opts]
            [runbld.process :as proc]
            [runbld.publish :as publish]
            [runbld.store :as store]
            [runbld.system :as system]
            [runbld.vcs :as vcs]
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
      env/wrap-env
      system/wrap-system))

;; -main :: IO ()
(defn -main [& args]
  (s/with-fn-validation
    (try+
     (let [opts (opts/parse-args args)
           _ (log ">>>>>>>>>>>> SCRIPT EXECUTION BEGIN >>>>>>>>>>>>")
           res (run opts)

           ;; _ build/wrap-test-report
           ;; _ store/wrap-save

           {:keys [took status exit-code
                   out-bytes err-bytes]} (:process res)

           _ (log "<<<<<<<<<<<< SCRIPT EXECUTION END   <<<<<<<<<<<<")]
       (log (format "DURATION: %sms" took))
       (log (format "STDOUT: %d bytes" out-bytes))
       (log (format "STDERR: %d bytes" err-bytes))
       (log
        (format "WRAPPED PROCESS: %s (%d)" status exit-code))

       (if (environ/env :dev)
         res
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
