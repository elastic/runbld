(ns runbld.vcs)

(defprotocol VcsRepo
  (log-latest [_] "Log map for latest commit")
  (check-out [_ commit] "Set working copy to version at commit")
  (fetch-latest [_] [_ & {:as opts-map}]
    "Updates local checkout to latest version")
  (is-shallow? [_])
  (provider [_] "What kind of VCS?"))
