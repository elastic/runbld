(ns runbld.build
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [clojure.java.io :as io]
            [environ.core :as environ]
            [runbld.util.data :refer [deep-merge-with deep-merge]]
            [runbld.util.date :as date]
            [runbld.vcs.git :as git]
            [slingshot.slingshot :refer [throw+]]))

(defn make-rand-uuid []
  (.toUpperCase
   (first
    (clojure.string/split
     (str (java.util.UUID/randomUUID))
     (re-pattern "-")))))

(defn make-id []
  (format "%s-%s"
          (date/yyyymmdd-hhmmss)
          (make-rand-uuid)))

(defn split-job-name
  [s]
  (when s
    (let [delim "+"
          [job-name org project branch job-name-extra]
          (re-find
           (re-pattern
            (format
             "^([^,]+)\\%s([^,]+)\\%s([^,]+)\\%s?([^,]*)?$"
             delim delim delim)) s)]
      {:org org
       :project project
       :branch branch
       :job-name-extra job-name-extra
       :org-project-branch (format "%s/%s#%s" org project branch)})))

(s/defn wrap-build-meta :- Opts4
  [proc :- clojure.lang.IFn]
  (fn [opts]
    (proc
     (assoc opts
            :id (make-id)
            :build (merge (:build opts)
                          (split-job-name (:job-name opts)))
            :jenkins {:url      (get-in opts [:env "BUILD_URL"])
                      :number   (get-in opts [:env "BUILD_NUMBER"])
                      :executor (get-in opts [:env "EXECUTOR_NUMBER"])
                      :node     (get-in opts [:env "NODE_NAME"])
                      :labels   (get-in opts [:env "NODE_LABELS"])}))))


