(ns runbld.build
  (:require [clojure.java.io :as io]
            [elasticsearch.document :as doc]
            [environ.core :as environ]
            [runbld.schema :refer :all]
            [runbld.util.data :refer [deep-merge-with deep-merge]]
            [runbld.util.date :as date]
            [runbld.scheduler :as scheduler]
            [runbld.vcs.middleware :as vcs]
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

(defn find-build [opts id]
  (-> (doc/search (-> opts :es :conn)
                  (-> opts :es :build-index-search)
                  {:body
                   {:query
                    {:match
                     {:id id}}}})
      :hits :hits first :_source))

(defn query-for-build [keyword-mapping? job-name vcs-provider]
  (let [clauses (if keyword-mapping?
                  ;; pre-5.0 mapping for string fields
                  [{:term {:build.job-name job-name}}
                   {:term {:process.status "SUCCESS"}}
                   {:term {:vcs.provider vcs-provider}}]
                  ;; post-5.0 mapping for string fields, before the
                  ;; mapping was updated in the code
                  [{:term {:build.job-name.keyword job-name}}
                   {:term {:process.status.keyword "SUCCESS"}}
                   {:term {:vcs.provider.keyword vcs-provider}}])]
    {:body
     {:query
      {:bool
       {:filter clauses}},
      :sort {:process.time-start {:order "desc"}},
      :size 1}}))

(defn find-last-good-build
  [keyword-mapping? conn idx job-name vcs-provider]
  (let [q (query-for-build keyword-mapping? job-name vcs-provider)
        doc (doc/search conn idx q)]
    (-> doc :hits :hits first :_source)))

(defn last-good-build
  "For a given project and branch, returns the last build known to
  have passed any job matching job-name."
  [job-name opts vcs-repo]
  (let [conn (-> opts :es :conn)
        idx (-> opts :es :build-index-search)
        build (fn [k?]
                ;; hardcode git for now; this is really supposed to
                ;; come from vcs/wrap-vcs-info, but that hasn't run
                ;; yet
                (find-last-good-build
                 k? conn idx job-name (runbld.vcs/provider vcs-repo)))]
    (or (build false)
        (build true))))

(defn maybe-find-last-good-build-and-checkout
  "Usually this should find /something/.

  Cases where you wouldn't find a last good build:

    * Fresh install
    * New --last-good-commit job name
    * Bogus --last-good-commit job name
  "
  [opts]
  (let [check-out? (boolean (:last-good-commit opts))
        job-name (if check-out?
                   (:last-good-commit opts)
                   (:job-name opts))
        vcs-repo (vcs/make-repo opts)]
    [(when-let [build (last-good-build job-name opts vcs-repo)]
       ;; only actually check out as working copy if the command line
       ;; opt has been supplied
       (when check-out?
         (runbld.vcs/check-out vcs-repo (-> build :vcs :commit-id)))
       build)
     check-out?]))

(defn abbreviate-last-good-build [last-good-build checked-out?]
  {:id (-> last-good-build :id)
   :checked-out checked-out?
   :commit-id (-> last-good-build :vcs :commit-id)
   :job-name (-> last-good-build :build :job-name)})

(s/defn wrap-build-meta :- OptsWithBuild
  [proc :- clojure.lang.IFn]
  (fn [opts*]
    (let [opts (assoc opts*
                      :id (make-id)
                      :build (merge (:build opts*)
                                    (split-job-name (:job-name opts*))
                                    (scheduler/as-map (:scheduler opts*))))
          [last-good-build checked-out?]
          (maybe-find-last-good-build-and-checkout opts)]
      (proc
       (update-in
        opts [:build] merge (when last-good-build
                              {:last-success
                               (abbreviate-last-good-build
                                last-good-build checked-out?)}))))))
