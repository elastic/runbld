(ns runbld.tests
  (:require [runbld.tests.junit]))

(defn capture-failures [workspace]
  (runbld.tests.junit/find-failures workspace))

(defn anything-to-report? [summary]
  (or (pos? (:errors   summary))
      (pos? (:failures summary))
      (pos? (:tests    summary))
      (pos? (:skipped  summary))))

(defn report [dir]
  (let [summary (capture-failures dir)]
    (if (anything-to-report? summary)
      {:report-has-tests true
       :report summary}
      {:report-has-tests false})))
