(ns runbld.test-support
  (:require
   [runbld.io :as io]
   [runbld.util.date :as date]
   [runbld.util.debug :as debug]))

(defn test-log [& x]
  (io/spit (.getAbsolutePath (clojure.java.io/file "target" "test.log"))
           (str (apply print-str x) (System/getProperty "line.separator"))
           :append true))

(defn redirect-logging-fixture
  "Redirects all io/log calls to a test.log file.  Intended to be used
  as an :each fixture as it will print a separator between each test."
  [f]
  (test-log "==========" (date/yyyymmdd-hhmmss) "==========")
  ;; Don't pollute the console
  (with-redefs [io/log test-log]
    (f)))

(defn reset-debug-log-fixture
  "Clears out the debug log between tests."
  [f]
  (debug/reset)
  (f))
