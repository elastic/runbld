(ns runbld.vcs.middleware
  (:require
   [clojure.java.io :as io]
   [environ.core :as environ]
   [runbld.schema :refer :all]
   [runbld.scm :as scm]
   [runbld.util.debug :as debug]
   [runbld.vcs :as vcs]
   [runbld.vcs.git :as git]
   [runbld.vcs.subversion :as svn]
   [schema.core :as s]
   [slingshot.slingshot :refer [throw+]]))

(s/defn make-repo :- (s/protocol vcs/VcsRepo)
  [opts]
  (let [source-dir (get-in opts [:process :cwd])]
    (cond
      (.isDirectory
       (io/file source-dir ".git")) (git/make-repo
                                     source-dir
                                     (get-in opts [:build :org])
                                     (get-in opts [:build :project])
                                     (get-in opts [:build :branch]))

      ;; TODO remove svn - it's not needed and is no longer 100% supported
      (.isDirectory
       (io/file source-dir ".svn")) (svn/make-repo
                                     (get-in opts [:env :SVN_URL])
                                     (get-in opts [:build :org])
                                     (get-in opts [:build :project])
                                     (get-in opts [:env :SVN_REVISION]))

      :else
      (let [msg (str source-dir ": unknown repository type "
                     "(only know about git and svn currently)")
            f (io/file source-dir)
            exists? (.exists f)]
        (debug/log msg
                   "CWD exists?" exists?
                   "Listing:" (if exists?
                                (vec (.list f))
                                "N/A")
                   "Process opts:" (:process opts))
        (throw+
         {:error ::unknown-repo
          :msg msg
          :opts opts})))))

(s/defn add-vcs-info
  [opts :- OptsWithBuild]
  (let [latest (vcs/log-latest (make-repo opts))]
    ((:logger opts)
     "Adding vcs info for the latest commit: " (:commit-id latest))
    (assoc opts :vcs latest)))
