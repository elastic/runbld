(ns runbld.vcs.middleware
  (:require [runbld.schema :refer :all]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+]])
  (:require [clojure.java.io :as io]
            [environ.core :as environ]
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

      :else (throw+
             {:error ::unknown-repo
              :msg (format (str
                            "%s: unknown repository type "
                            "(only know about git and svn currently)")
                           cwd)
              :opts opts}))))

(s/defn wrap-vcs-info :- MainOpts
  [proc :- clojure.lang.IFn]
  (s/fn [opts :- OptsWithBuild]
    (proc
     (assoc opts :vcs (vcs/log-latest
                       (make-repo opts))))))
