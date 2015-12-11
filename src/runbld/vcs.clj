(ns runbld.vcs)

(defprotocol VcsRepo
  (log-latest [this] "Log for recent commit")
  (log [this commit] "Log for commit"))

