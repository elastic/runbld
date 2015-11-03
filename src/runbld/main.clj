(ns runbld.main
  (:gen-class)
  (:require [clojure.pprint :refer [pprint]]
            [runbld.build :as build]
            [runbld.env :as env]
            [runbld.opts :as opts]
            [runbld.process :as proc]
            [runbld.publish :as publish]
            [slingshot.slingshot :refer [try+]]))

(defn really-die [code strmsg]
  (println strmsg)
  (System/exit code))

(defn die [code msg]
  (let [msg* (.trim (with-out-str (println msg)))]
    (really-die code msg*)
    ;; for tests when #'really-die is redefed
    msg*))

(defn log [& s]
  (apply println s))

(defn wrap-execute [proc]
  (fn [opts]
    (assoc opts :proc (proc (:scriptfile opts)))))

(def run
  (-> (wrap-execute #'proc/run)
      publish/wrap-publish
      build/wrap-build-meta
      env/wrap-env))

;; -main :: IO ()
(defn -main [& args]
  (try+
   (let [opts (opts/parse-args args)
         _ (log ">>>>>>>>>>>> SCRIPT EXECUTION BEGIN >>>>>>>>>>>>")
         res (run opts)
         {:keys [took status] :as proc} (:proc res)
         _ (log "<<<<<<<<<<<< SCRIPT EXECUTION END   <<<<<<<<<<<<")]
     (assert status "process did not return a status key")
     (log (format "DURATION: %sms" took))
     (log
      (format "WRAPPER: %s (%d)" (if (zero? status)
                                   "SUCCESS"
                                   "FAILURE") status))
     ;; for tests
     proc)

   (catch [:error :runbld.opts/parse-error] {:keys [msg]}
     (die 2 msg))

   (catch [:error :runbld.opts/file-not-found] {:keys [msg]}
     (die 2 msg))

   (catch [:help :runbld.opts/version] {:keys [msg]}
     (die 0 msg))

   (catch [:help :runbld.opts/usage] {:keys [msg]}
     (die 0 msg))

   (catch Exception e
     (die 1 e))))
