(ns runbld.vcs)

(defprotocol VcsRepo
  (log-latest [_] "Log map for latest commit")
  (provider [_] "What kind of VCS?"))

