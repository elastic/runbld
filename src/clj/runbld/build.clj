(ns runbld.build
  (:require [clojure.java.io :as io]
            [elasticsearch.document :as doc]
            [environ.core :as environ]
            [runbld.schema :refer :all]
            [runbld.util.data :refer [deep-merge-with deep-merge]]
            [runbld.util.date :as date]
            [runbld.scheduler :as scheduler]
            [schema.core :as s]
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
             "^([^%s]+)\\%s([^%s]+)\\%s([^%s]+)\\%s?([^%s]*)?$"
             delim delim delim delim delim delim delim)) s)]
      {:job-name job-name
       :org org
       :project project
       :branch branch
       :job-name-extra job-name-extra
       :org-project-branch (format "%s/%s#%s" org project branch)})))

(defn query-for-commit [keyword-mapping? org-project-branch vcs-provider]
  (let [clauses (if keyword-mapping?
                  ;; post-5.0 mapping for string fields
                  [{:term {:build.org-project-branch org-project-branch}}
                   {:term {:process.status.keyword "SUCCESS"}}
                   {:term {:vcs.provider vcs-provider}}]
                  ;; pre-5.0 mapping for string fields
                  [{:term {:build.org-project-branch org-project-branch}}
                   {:term {:process.status "SUCCESS"}}
                   {:term {:vcs.provider vcs-provider}}])]
    {:body
     {:query
      {:bool
       {:filter clauses}},
      :sort {:process.time-start {:order "desc"}},
      :size 1}}))

(defn find-last-good-build
  [keyword-mapping? conn idx org-project-branch vcs-provider]
  (let [q (query-for-commit keyword-mapping? org-project-branch vcs-provider)
        doc (doc/search conn idx q)]
    (-> doc
        :hits
        :hits
        first
        :_source)))

(defn last-good-commit
  "For a given project and branch, returns the last commit ID known to
  have passed any job matching org/project/branch"
  [{:keys [job-name] :as opts}]
  (let [conn (-> opts :es :conn)
        idx (-> opts :es :build-index-search)
        org-project-branch (:org-project-branch (split-job-name job-name))
        build (fn [k?]
                ;; hardcode git for now; this is really supposed to
                ;; come from vcs/wrap-vcs-info, but that hasn't run
                ;; yet
                (find-last-good-build k? conn idx org-project-branch "git"))]
    (or (build false)
        (build true))))

(s/defn wrap-build-meta :- OptsWithBuild
  [proc :- clojure.lang.IFn]
  (fn [opts]
    (proc
     (assoc opts
            :id (make-id)
            :build (merge (:build opts)
                          (split-job-name (:job-name opts))
                          (scheduler/as-map (:scheduler opts)))))))
