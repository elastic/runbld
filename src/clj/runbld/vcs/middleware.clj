(ns runbld.vcs.middleware
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [clojure.java.io :as io]
            [environ.core :as environ]
            [runbld.vcs :as vcs]
            [runbld.vcs.subversion :as svn]
            [runbld.vcs.git :as git]))

(s/defn make-repo :- (s/protocol vcs/VcsRepo)
  [opts :- OptsStage5]
  (let [cwd (get-in opts [:process :cwd])]
    (cond
      (.isDirectory
       (io/file cwd ".git")) (git/make-repo
                              cwd
                              (if (environ/env :dev)
                                "http://example.com"
                                (get-in opts [:env "GIT_URL"]))
                              (get-in opts [:build :org])
                              (get-in opts [:build :project])
                              (get-in opts [:build :branch]))

      (.isDirectory
       (io/file cwd ".svn")) (svn/make-repo
                              (get-in opts [:env "SVN_URL"])
                              (get-in opts [:build :org])
                              (get-in opts [:build :project])
                              (get-in opts [:build :branch])
                              (get-in opts [:env "SVN_REVISION"]))

      :else (throw
             (Exception.
              (format (str
                       "%s: unknown repository type "
                       "(only know about git and svn currently)")
                      cwd))))))

(s/defn wrap-vcs-info :- OptsFinal
  [proc :- clojure.lang.IFn]
  (fn [opts]
    (proc
     (assoc opts :vcs (vcs/log-latest
                       (make-repo opts))))))
