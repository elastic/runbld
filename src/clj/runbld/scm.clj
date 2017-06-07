(ns runbld.scm
  (:require
   [clj-git.core :as git]
   [clojure.java.io :as jio]
   [runbld.io :as io]
   [runbld.schema :refer :all]
   [schema.core :as s]))

(defn find-workspace
  "just to override in tests"
  []
  (System/getenv "WORKSPACE"))

(s/defn wipe-workspace
  [opts :- {(s/optional-key :scm) OptsScm
            s/Keyword s/Any}]
  (when (boolean (-> opts :scm :wipe-workspace))
    (let [workspace (find-workspace)]
      (io/log "wiping workspace" workspace)
      (io/rmdir-contents workspace)))
  opts)

(defn update-workspace
  "Updates an existing workspace and gets it onto the correct
  branch."
  [local branch depth]
  (let [repo (git/load-repo local)]
    (io/log "repo already cloned."
            "local:" local
            "branch:" branch
            "depth:" depth
            "repo:" repo)
    (io/log "updating")
    (git/git-remote repo ["set-branches" "origin" branch])
    (let [fetch-args (concat (when depth
                               ["--depth" depth])
                             ["origin" branch])]
      (git/git-fetch repo fetch-args))
    (git/git-checkout repo branch)
    (git/git-pull repo)
    (io/log "done updating")))

(s/defn bootstrap-workspace
  [opts :- {:process OptsProcess
            :build Build
            (s/optional-key :scm) OptsScm
            s/Keyword s/Any}]
  (let [clone? (boolean (-> opts :scm :clone))
        local (-> opts :process :cwd)
        remote (-> opts :scm :url)
        reference (-> opts :scm :reference-repo)
        branch (or (-> opts :scm :branch)
                   (-> opts :build :branch))
        depth (-> opts :scm :depth)]
    (when clone?
      (if (.exists (jio/file local ".git"))
        (update-workspace local branch depth)
        (let [clone-args (->> [(when (and reference
                                          (.exists (jio/as-file reference)))
                                 ["--reference" reference])
                               (when branch ["--branch" branch])
                               (when depth ["--depth" depth])]
                              (filter identity)
                              (apply concat))]
          (io/log "cloning" remote)
          (git/git-clone local remote clone-args)
          (io/log "done cloning")))))
  opts)
