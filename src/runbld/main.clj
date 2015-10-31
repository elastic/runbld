(ns runbld.main
  (:gen-class)
  (:require [clojure.pprint :refer [pprint]]
            [runbld.opts :as opts]
            [runbld.process :as proc]
            [slingshot.slingshot :refer [try+]]))

(defn really-die [code strmsg]
  (println strmsg)
  (System/exit code))

(defn die [code msg]
  (let [msg* (.trim (with-out-str (println msg)))]
    (really-die code msg*)
    ;; for tests when #'really-die is redefed
    msg*))

(defn log [s]
  (println s))

;; -main :: IO ()
(defn -main [& args]
  (try+
   (let [opts (opts/parse-args args)
         _ (log ">>>>>>>>>>>> SCRIPT EXECUTION BEGIN >>>>>>>>>>>>")
         {:keys [duration-millis status]
          :as res} (proc/run (opts :scriptfile))
         _ (log "<<<<<<<<<<<< SCRIPT EXECUTION END   <<<<<<<<<<<<")]
     (log (format "DURATION: %sms" duration-millis))
     (log
      (format "WRAPPER: %s (%d)" (if (zero? status)
                                   "SUCCESS"
                                   "FAILURE") status))
     res)

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
