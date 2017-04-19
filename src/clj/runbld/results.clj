(ns runbld.results
  (:require [runbld.results.gradle :as gradle]))

(defn summary [conn idx build-id]
  (when-let [s (gradle/went-wrong conn idx build-id)]
    {:summary s
     :lines (gradle/failure-lines conn idx build-id)}))
