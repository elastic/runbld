(ns runbld.scm
  (:require
   [clj-git.core :as git]
   [clj-time.core :as t]
   [clojure.java.io :as jio]
   [clojure.string :as string]
   [environ.core :as environ]
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
      (io/log "Wiping workspace." workspace)
      (io/rmdir-contents workspace)))
  opts)

(defn checkout-commit
  [local commit]
  (let [repo (git/load-repo local)]
    (io/log "Fetching provided commit" commit)
    (when (git/shallow-clone? repo)
      (io/log "Repo was shallow.  Fetching all commits newer than 6 months.")
      (git/git-fetch repo [(str "--shallow-since="
                                (str (t/minus (t/now) (t/months 6))))]))
    (git/git-checkout repo commit)
    (io/log "Done fetching.")))

(defn update-workspace
  "Updates an existing workspace and gets it onto the correct
  branch."
  [local branch depth]
  (let [repo (git/load-repo local)]
    (io/log "Repo already cloned to" local)
    (io/log "Updating branch to" branch "with depth" depth)
    (git/git-remote repo ["set-branches" "origin" branch])
    (let [fetch-args (concat (when depth
                               ["--depth" depth])
                             ["origin" branch])]
      (git/git-fetch repo fetch-args))
    (git/git-checkout repo branch)
    (io/log "Done updating branch.")))

(defn clone-workspace
  "Clones a git repo at the specified branch and depth.  If the local
  reference repo is available, clone from there to save time."
  [local remote reference branch depth]
  (let [use-reference-repo? (and reference
                                 (.exists (jio/as-file reference)))
        clone-args (->> [(when use-reference-repo?
                           ["--reference" reference])
                         (when branch ["--branch" (str branch)])
                         (when depth ["--depth" (str depth)])]
                        (filter identity)
                        (apply concat))]
    (io/log "Cloning repo" remote
            "to" local
            "on branch" branch
            "with depth" depth)
    (when use-reference-repo?
      (io/log "Using local reference repo at" reference))
    (git/git-clone local remote clone-args)
    (io/log "Done cloning.")))

(defn choose-branch
  "Select the branch (or commit) to checkout.  The branch is chosen
  from the environment (i.e., where Jenkins puts build parameters), or
  from the scm config, or finally from the job name.  Since Jenkins
  always injects parameters we will ignore them if the injected value
  is the default which is 'refs/heads/{branch-from-job-name}' by
  convention.

  In the case where commits are present, the commit in the highest
  priority position will be chosen along with the first branch
  found (or master as a default).

  non-default-env > scm-config > job-name"
  [opts]
  (let [;; the branch name, if any, specified in the scm opts
        scm-branch (-> opts :scm :branch)
        ;; the branch name parsed out of the job name
        build-branch (-> opts :build :branch)
        ;; the branch name injected into the env by Jenkins
        env-branch (environ/env :branch-specifier)
        ;; the default branch that Jenkins will send unless it's overridden
        default-env-branch (str "refs/heads/" build-branch)
        commit? (fn [x]
                  (and
                   (string? x)
                   (boolean
                    (re-matches #"^[a-fA-F0-9]{5,40}$"
                                (string/trim x)))))]
    (let [branches (cond->> [build-branch]
                     (not (empty? scm-branch))
                     (cons scm-branch)

                     (and (not (empty? env-branch))
                          (not= default-env-branch env-branch))
                     (cons env-branch))
          commit (first (take-while commit? branches))
          branch (first (drop-while commit? branches))]
      {:commit commit :branch (or branch "master")})))

(defn clean-branch-name [branch]
  (-> branch
      (string/replace #"^refs/heads/" "")
      (string/replace #"^origin/" "")))

(defn checkout-dir [opts]
  (let [cwd (-> opts :process :cwd)]
    (if-let [basedir (-> opts :scm :basedir)]
      (string/replace (string/join "/" [cwd basedir]) #"/+" "/")
      cwd)))

(s/defn bootstrap-workspace
  "Prepare the local workspace by cloning or updating the repository,
  as necessary."
  [opts :- {:process OptsProcess
            :build Build
            (s/optional-key :scm) OptsScm
            s/Keyword s/Any}]
  (let [clone? (boolean (-> opts :scm :clone))
        local (checkout-dir opts)
        remote (-> opts :scm :url)
        reference (-> opts :scm :reference-repo)
        {:keys [branch commit]} (choose-branch opts)
        depth (-> opts :scm :depth)]
    (when clone?
      (if (.exists (jio/file local ".git"))
        (update-workspace local branch depth)
        (clone-workspace local remote reference branch depth))
      (when commit
        (checkout-commit local commit)))
    ;; Update the build data
    (assoc-in opts [:build :branch] (clean-branch-name branch))))
