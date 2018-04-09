(ns runbld.build
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [elasticsearch.document :as doc]
   [environ.core :as environ]
   [runbld.io :as rio]
   [runbld.scheduler :as scheduler]
   [runbld.schema :refer :all]
   [runbld.util.data :refer [deep-merge-with deep-merge]]
   [runbld.util.date :as date]
   [runbld.util.debug :as debug]
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
    (let [[org project branch job-name-extra] (string/split s #"\+" 4)]
      {:job-name s
       :org (or org "")
       :project (or project "")
       :branch (or branch "")
       :job-name-extra (or job-name-extra "")
       :org-project-branch (format "%s/%s#%s" org project branch)})))

(defn find-build [opts id]
  (when id
    (-> (doc/search (-> opts :es :conn)
                    (-> opts :es :build-index-search)
                    {:body
                     {:query
                      {:match
                       {:id id}}}})
        :hits :hits first :_source)))

(defn query-for-build [keyword-mapping? job-name vcs-provider]
  (let [clauses
        [{:term {:build.job-name job-name}}
         {:term {:process.status "SUCCESS"}}
         {:bool {:minimum_should_match 1
                 :should
                 [{:term {:build.user-specified-branch false}}
                  {:bool {:must_not
                          {:exists
                           {:field :build.user-specified-branch}}}}]}}]]
    {:body
     {:query
      {:bool
       {:filter clauses}},
      ;; search for time-start, because we want the latest commit that
      ;; has a *finished* job
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

(defn checkout-last-good-commit
  "Checks out the commit id found in the last good build.  Will deepen
  a shallow clone to include that commit, if necessary."
  [vcs-repo {{:keys [commit-id commit-time]} :vcs :as build}]
  (if (runbld.vcs/is-shallow? vcs-repo)
    (runbld.vcs/fetch-latest vcs-repo :shallow-since commit-time)
    (runbld.vcs/fetch-latest vcs-repo))
  (runbld.vcs/check-out vcs-repo commit-id))

(defn maybe-find-last-good-build-and-checkout
  "Usually this should find /something/.

  Cases where you wouldn't find a last good build:

    * Fresh install
    * New --last-good-commit job name
    * Bogus --last-good-commit job name"
  [opts]
  (let [check-out? (boolean (:last-good-commit opts))
        job-name (if check-out?
                   (:last-good-commit opts)
                   (:job-name opts))
        vcs-repo (vcs/make-repo opts)]
    [(when-let [build (last-good-build job-name opts vcs-repo)]
       (debug/log "Found last-good-commit:" (-> build :vcs :commit-id))
       ;; only actually check out as working copy if the command line
       ;; opt has been supplied
       (when check-out?
         (checkout-last-good-commit vcs-repo build))
       build)
     check-out?]))

(defn abbreviate-last-good-build [last-good-build checked-out?]
  {:id (-> last-good-build :id)
   :checked-out checked-out?
   :commit-id (-> last-good-build :vcs :commit-id)
   :job-name (-> last-good-build :build :job-name)})

(defn set-build-meta-environment
  "If last-good-commit was specified, this reads the build metadata
  from the commit and stores it in the BUILD_METADATA variable in the
  script execution environment."
  [opts]
  (let [env-metadata (environ/env :build-metadata)]
    (if (not (string/blank? env-metadata))
      (do
        ((:logger opts) "BUILD_METADATA found in the environment.")
        ((:logger opts) "BUILD_METADATA:" env-metadata)
        (assoc-in opts [:process :env :BUILD_METADATA] env-metadata))
      (if (-> opts :build :last-success :checked-out)
        (let [build (find-build opts (-> opts :build :last-success :id))
              metadata (-> build :build :metadata)]
          (if (string/blank? metadata)
            (do
              ((:logger opts)
               "No build metadata found, not setting BUILD_METADATA")
              opts)
            (do
              ((:logger opts) "BUILD_METADATA found in Elasticsearch.")
              ((:logger opts) "BUILD_METADATA:" metadata)
              (assoc-in opts [:process :env :BUILD_METADATA] metadata))))
        opts))))

(defn record-build-meta
  "Searches the build workspace for files starting with build_metadata
  and concats their contents and stores it in the build metadata in
  Elasticsearch."
  [{:keys [logger] :as opts}]
  (logger "Searching for build metadata in" (System/getProperty "user.dir"))
  (let [prep-content #(-> %
                          string/trim
                          (string/replace #"\n+$" "")
                          ;; strip trailing semicolons so we can blindly
                          ;; str/join
                          (string/replace #";+$" ""))
        metadata (->> (rio/find-files
                       (System/getProperty "user.dir")
                       #"/build_metadata")
                      (map slurp)
                      (map prep-content)
                      (string/join ";"))]
    (logger "Storing build metadata:" metadata)
    (assoc-in opts [:build :metadata] metadata)))


;; Pipeline functions


(s/defn add-build-id :- OptsWithId
  [opts :- Opts]
  (let [build-id (make-id)]
    (debug/log "Build id:" build-id)
    (assoc opts :id build-id)))

(s/defn add-build-info :- OptsWithBuild
  [opts :- {:job-name s/Str
            :scheduler (s/protocol scheduler/Scheduler)
            s/Keyword s/Any}]
  (let [build-meta (merge (split-job-name (:job-name opts))
                          (scheduler/as-map (:scheduler opts)))]
    (debug/log "Build meta:" build-meta)
    (assoc opts
           :process-result {:time-start (date/ms-to-iso)}
           :build build-meta)))

(s/defn add-last-success
  [opts :- OptsWithBuild]
  (if (get-in opts [:build :user-specified-branch])
    ;; The user specified a specific branch or commit and runbld
    ;; shouldn't override it with the last-good-commit
    opts
    (let [[last-good-build checked-out?]
          (maybe-find-last-good-build-and-checkout opts)]
      (cond-> opts
        last-good-build
        (update :build merge {:last-success (abbreviate-last-good-build
                                             last-good-build checked-out?)})

        (and last-good-build checked-out?)
        (update :vcs merge
                (:vcs (find-build opts (:id last-good-build))))))))

(s/defn maybe-log-last-success
  [opts :- (merge {:logger clojure.lang.IFn}
                  OptsWithBuild)]
  (when (-> opts :build :last-success :checked-out)
    (let [b (find-build opts (-> opts :build :last-success :id))
          commit (-> b :vcs :commit-short)]
      ((:logger opts)
       "Using last successful commit" commit
       "from the job" (-> b :build :job-name)
       "with the build id" (-> b :id)
       "that was started at" (-> b :process :time-start)
       "and finished" (date/human-duration
                       (date/iso-diff-secs
                        (date/from-iso
                         ;; notify on time-end because it makes more
                         ;; logical sense to report on the last
                         ;; completed build's end time, I think
                         (-> b :process :time-end))
                        (date/now)))
       "ago")))
  opts)

(defn add-build-meta
  [proc opts]
  (-> opts
      set-build-meta-environment
      proc
      record-build-meta))
