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
        (proc (assoc opts :build (merge (:build opts) {:commit "fake"})))
        (throw+ {:error ::no-git-remote
                 :msg (format "no remote set for %s"
                              (with-out-str
                                (pr (:build opts))))}))
      (let [{:keys [org project workspace branch]} (:build opts)
            {:keys [clone-home remote]} (:git opts)
            commit (git/checkout-workspace
                    clone-home remote
                    workspace org project (format "origin/%s" branch))]
        (proc (assoc opts :build (merge (:build opts) commit)))))))

(defn wrap-merge-profile [proc]
  (fn [opts]
    (let [profile-name (keyword (get-in opts [:build :profile-name]))
          profile (or (get-in opts [:profiles profile-name]) {})]
      (proc (deep-merge-with deep-merge opts profile)))))

(defn wrap-build-meta [proc]
  (fn [opts]
    (let [info (inherited-build-info
                (or (get-in opts [:env "JOB_NAME"])
                    (get-in opts [:build :job-name])))
          opts* (assoc
                 opts
                 :build (merge
                         {:id (make-id)
                          :url            (get-in opts [:env "BUILD_URL"])
                          :jenkins-number (get-in opts [:env "BUILD_NUMBER"])
                          :node-executor  (get-in opts [:env "EXECUTOR_NUMBER"])
                          :host           (get-in opts [:env "NODE_NAME"])
                          :labels         (get-in opts [:env "NODE_LABELS"])
                          :workspace      (or
                                           (get-in opts [:env "WORKSPACE"])
                                           ;; useful in tests, or the
                                           ;; non-Jenkins future
                                           (str
                                            (io/file
                                             (get-in opts [:build :workspace-home])
                                             (:job-name info))))}
                         info))]
      (proc opts*))))
