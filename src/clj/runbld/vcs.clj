(ns runbld.vcs)

(defprotocol VcsRepo
  (log-latest [_] "Log map for latest commit")
  (vendor [_] "What kind of VCS?"))

