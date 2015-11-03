(ns runbld.build
  (:require [runbld.util.date :as date]))

(defn make-rand-uuid []
  (.replaceAll (str (java.util.UUID/randomUUID)) "-" ""))

(defn make-id []
  (format "%s-%s"
          (date/yyyymmdd-hhmmss)
          (make-rand-uuid)))

(defn split-jenkins-name [s]
  (let [[_ org project branch] (re-find #"^([^,]+),([^,]+),([^,]+)$" s)]
    {:org org
     :project project
     :branch branch}))

(defn inherited-build-info [opts]
  (if (get-in opts [:env "JENKINS_HOME"])
    (split-jenkins-name (get-in opts [:env "JOB_NAME"]))))

(defn wrap-build-meta [proc]
  (fn [opts]
    (proc (assoc opts
                 :build (merge
                         {:id (make-id)}
                         (inherited-build-info opts))))))
