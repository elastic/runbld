(ns runbld.vcs)

(defprotocol VcsRepo
  (log-latest [_] "Log map for latest commit")
  (check-out [_ commit] "Set working copy to version at commit")
  (fetch-latest [_] "Updates local checkout to latest version")
  (provider [_] "What kind of VCS?"))
