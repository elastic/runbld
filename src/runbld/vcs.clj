(ns runbld.vcs
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [runbld.vcs.repo :as repo]))

(s/defrecord VcsLog
    [
     commit-id     :- s/Str
     message-short :- s/Str
     message-full  :- s/Str
     commit-name   :- s/Str
     commit-email  :- s/Str
     commit-time   :- s/Str
     author-name   :- s/Str
     author-email  :- s/Str
     author-time   :- s/Str
     ])

(defprotocol VcsRepo
  (log-latest [this] "Log for recent commit")
  (log [this commit] "Log for commit"))

(defn wrap-vcs-info [proc]
  (fn [opts]
    (let [repo (repo/make-repo (get-in opts [:process :cwd]))
          opts (assoc opts :vcs (log-latest repo))]
      (proc opts))))
