(ns runbld.vcs.middleware
  (:require [runbld.schema :refer :all]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+]])
  (:require [clojure.java.io :as io]
            [environ.core :as environ]
            [runbld.util.debug :as debug]
            [runbld.vcs :as vcs]
            [runbld.vcs.subversion :as svn]
            [runbld.vcs.git :as git]))

(s/defn make-repo :- (s/protocol vcs/VcsRepo)
  [opts]
  (let [cwd (get-in opts [:process :cwd])]
    (cond
      (.isDirectory
       (io/file cwd ".git")) (git/make-repo
                              cwd
                              (get-in opts [:build :org])
                              (get-in opts [:build :project])
                              (get-in opts [:build :branch]))

      (.isDirectory
       (io/file cwd ".svn")) (svn/make-repo
                              (get-in opts [:env :SVN_URL])
                              (get-in opts [:build :org])
                              (get-in opts [:build :project])
                              (get-in opts [:env :SVN_REVISION]))

      :else
      (let [msg (str cwd ": unknown repository type "
                     "(only know about git and svn currently)")
            f (io/file cwd)
            exists? (.exists f)]
        (debug/log msg
                   "CWD exists?" exists?
                   "Listing:" (if exists?
                                (.list f)
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
