(ns runbld.test-support
  (:require
   [clojure.java.io :as jio]
   [runbld.io :as io]
   [runbld.main :as main]
   [runbld.util.date :as date]
   [runbld.util.debug :as debug])
  (:import (java.io FileOutputStream)))

(def log-file (clojure.java.io/file
               (System/getProperty "user.dir")
               "target"
               "test.log"))

(defn test-log
  [log-atom]
  (fn [& x]
    (let [log-msg (str (apply print-str x)
                       (System/getProperty "line.separator"))]
      (when log-atom
        (swap! log-atom conj log-msg))
      (io/spit (.getAbsolutePath log-file)
               log-msg
               :append true))))

(defn redirect-logging-fixture
  "Redirects all io/log calls to a test.log file.  Intended to be used
  as an :each fixture as it will print a separator between each test."
  ([]
   (redirect-logging-fixture nil))
  ([log-atom]
   (fn [f]
     ;; Don't pollute the console
     (let [w (jio/writer log-file :append true)]
       (binding [runbld.process/*process-err* w
                 runbld.process/*process-out* w]
         (with-redefs [io/log (test-log log-atom)]
           (io/log "==========" (date/yyyymmdd-hhmmss) "==========")
           (f)))))))

(defn reset-debug-log-fixture
  "Clears out the debug log between tests."
  [f]
  (debug/reset)
  (f))

(defn dont-die-fixture [f]
  (with-redefs [main/really-die
                (fn [& args]
                  ;; tattle if someone tries to kill the JVM
                  (println "SOMEBODY TRIED TO KILL THE JVM!" args)
                  :dontdie)]
    (f)))
