(ns runbld.build
  (:require [clojure.java.io :as io]
            [environ.core :as environ]
            [runbld.util.data :refer [deep-merge-with deep-merge]]
            [runbld.util.date :as date]
            [runbld.vcs.git :as git]
            [slingshot.slingshot :refer [throw+]]))

(defn make-rand-uuid []
  (.replaceAll (str (java.util.UUID/randomUUID)) "-" ""))

(defn make-id []
  (format "%s-%s"
          (date/yyyymmdd-hhmmss)
          (make-rand-uuid)))

(defn split-job-name
  [s]
  (when s
    (let [[job-name org project branch job-name-extra]
          (re-find #"^([^,]+),([^,]+),([^,]+),?([^,]*)?$" s)]
      {:org org
       :project project
       :branch branch
       :job-name-extra job-name-extra
       :job-name job-name
       :profile-name (format "%s-%s-%s" org project branch)})))

(defn inherited-build-info [raw-name]
  (let [{:keys [job-name] :as info} (split-job-name raw-name)]
    (if (or job-name (environ/env :dev))
      info
      (throw+ {:error ::invalid-job-name
               :msg "please set $JOB_NAME in the format 'org,repo,branch'"}))))

(defn wrap-git-repo [proc]
  (fn [opts]
    (if (nil? (get-in opts [:git :remote]))
      (if (environ/env :dev)
        (proc (update opts :build merge {:commit "fake"}))
        (throw+ {:error ::no-git-remote
                 :msg (format "no remote set for %s"
                              (with-out-str
                                (pr (:build opts))))}))
      (let [{:keys [org project workspace branch]} (:build opts)
            {:keys [clone-home remote]} (:git opts)
            commit (git/checkout-workspace
                    clone-home remote
                    workspace org project (format "origin/%s" branch))]
        (proc
         (-> opts
             (update :build merge commit)
             (update :process merge {:cwd workspace})))))))

(defn wrap-merge-profile [proc]
  (fn [opts]
    (let [profile-name (keyword (get-in opts [:build :profile-name]))
          profile (or (get-in opts [:profiles profile-name]) {})]
      (proc (deep-merge-with deep-merge opts profile)))))

(defn wrap-build-meta [proc]
  (fn [opts]
    (let [info (inherited-build-info
                (get-in opts [:build :job-name]))
          id (make-id)
          opts* (assoc
                 opts
                 :build (merge
                         {:id id
                          :url            (get-in opts [:env "BUILD_URL"])
                          :jenkins-number (get-in opts [:env "BUILD_NUMBER"])
                          :jenkins-executor (get-in
                                             opts [:env "EXECUTOR_NUMBER"])
                          :jenkins-node   (get-in opts [:env "NODE_NAME"])
                          :jenkins-labels (get-in opts [:env "NODE_LABELS"])
                          :workspace
                          (str
                           (io/file
                            (get-in opts [:build :workspace-home])
                            (format "%s-%s"
                                    id
                                    (:profile-name info))))}
                         info))]
      (proc opts*))))
