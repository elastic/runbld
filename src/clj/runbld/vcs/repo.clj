(ns runbld.vcs.repo
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [clojure.java.io :as io]
            [runbld.vcs :as vcs]
            [runbld.vcs.subversion :as svn]
            [runbld.vcs.git :as git]))

(s/defn make-repo
  [opts :- Opts4]
  (let [cwd (get-in opts [:process :cwd])]
    (cond
      (.isDirectory
       (io/file cwd ".git")) (git/make-repo cwd)

      (.isDirectory
       (io/file cwd ".svn")) (svn/make-repo
                              (get-in opts [:env "SVN_URL"])
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
