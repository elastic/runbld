(ns runbld.vcs)

(defprotocol VcsRepo
  (log-latest [_] "Log map for latest commit")
  (check-out [_ commit] "Set working copy to version at commit")
  (provider [_] "What kind of VCS?"))

