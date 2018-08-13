(ns runbld.tests
  (:require
   [runbld.schema :refer :all]
   [runbld.tests.junit]
   [runbld.util.debug :as debug]
   [schema.core :as s]))

(defn capture-failures [workspace]
  (debug/log "finding failures")
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

(s/defn add-test-report :- {:test-report TestReport
                            s/Keyword s/Any}
  [opts :- {:process OptsProcess
            s/Keyword s/Any}]
  (debug/log "Add Test Report")
  (assoc opts :test-report (report (-> opts :process :cwd))))
