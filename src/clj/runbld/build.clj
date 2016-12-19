(ns runbld.build
  (:require [clojure.java.io :as io]
            [clojure.spec :as s]
            [environ.core :as environ]
            [runbld.util.data :refer [deep-merge-with deep-merge]]
            [runbld.util.date :as date]
            [runbld.scheduler :as scheduler]
            [slingshot.slingshot :refer [throw+]]))

(s/def ::id string?)
(s/def ::org string?)
(s/def ::project string?)
(s/def ::branch string?)
(s/def ::job-name-extra string?)
(s/def ::job-name string?)
(s/def ::org-project-branch string?)
(s/def ::scheduler string?)
(s/def ::url string?)
(s/def ::console-url string?)
(s/def ::tags (s/coll-of string?))
(s/def ::number string?)
(s/def ::executor string?)
(s/def ::node string?)

(s/def ::build
  (s/keys :req-un [::org
                   ::project
                   ::branch
                   ::job-name-extra
                   ::job-name
                   ::org-project-branch
                   ::scheduler
                   ::url
                   ::console-url
                   ::tags]
          :opt-un [::number
                   ::executor
                   ::node]))

(s/def ::job-split
  (s/keys :req-un [::job-name
                   ::org
                   ::project
                   ::branch
                   ::job-name-extra
                   ::org-project-branch]))

(s/fdef make-rand-uuid
        :args (s/coll-of any? :count 0)
        :ret string?)
(defn make-rand-uuid []
  (.toUpperCase
   (first
    (clojure.string/split
     (str (java.util.UUID/randomUUID))
     (re-pattern "-")))))

(s/fdef make-id
        :args (s/coll-of any? :count 0)
        :ret string?)
(defn make-id []
  (format "%s-%s"
          (date/yyyymmdd-hhmmss)
          (make-rand-uuid)))

(s/fdef split-job-name
        :args (s/cat :s string?)
        :ret (s/nilable ::job-split))
(defn split-job-name
  [s]
  (when s
    (let [delim "+"
          [job-name org project branch job-name-extra]
          (re-find
           (re-pattern
            (format
             "^([^%s]+)\\%s([^%s]+)\\%s([^%s]+)\\%s?([^%s]*)?$"
             delim delim delim delim delim delim delim)) s)]
      (when job-name
        {:job-name job-name
         :org org
         :project project
         :branch branch
         :job-name-extra job-name-extra
         :org-project-branch (format "%s/%s#%s" org project branch)}))))

(defn wrap-build-meta
  [proc]
  (fn [opts]
    (proc
     (assoc opts
            :id (make-id)
            :build (merge (:build opts)
                          (split-job-name (:job-name opts))
                          (scheduler/as-map (:scheduler opts)))))))

