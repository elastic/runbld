(ns runbld.results
  (:require [runbld.results.gradle :as gradle]))

(defn summary [conn idx build-id]
  (when-let [s (or (gradle/failed-task conn idx build-id)
                   (gradle/went-wrong conn idx build-id))]
    {:summary s
     :lines (gradle/failure-lines conn idx build-id)}))
