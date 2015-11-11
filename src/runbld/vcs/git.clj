(ns runbld.vcs.git
  (:require [clj-jgit.porcelain :as git]
            [clojure.java.io :as jio]
            [clojure.java.shell :as sh]
            [environ.core :as environ]
            [runbld.util.date :as date]
            [runbld.util.io :as io]
            [slingshot.slingshot :refer [throw+]]))

(defn repo? [dir]
  (when dir
    (.isDirectory (jio/file dir ".git"))))

(defn init-test-repo [dir]
  (assert (not (nil? dir)))
  (if (repo? dir)
    (git/load-repo dir)
    (let [basename "this-is-a-test-repo.sh"
          f (jio/file dir basename)
          repo (git/git-init dir)]
      (spit f "echo 'this would have been a test build --->here<---'\n")
      (git/git-add repo basename)
      (git/git-commit repo "Add build")
      repo)))

(defn add-test-commit [dir]
  (let [repo (git/load-repo dir)
        basename (str (java.util.UUID/randomUUID) ".txt")
        f (jio/file dir basename)]
    (spit f "test")
    (git/git-add repo basename)
    (git/git-commit repo (format "Add %s!" basename))))

(defn commit-map [commit]
  (let [author (.getAuthorIdent commit)
        committer (.getCommitterIdent commit)]
    {:commit (.getName commit)
     :commit-desc (.getShortMessage commit)
     :commit-msg (.getFullMessage commit)
     :commit-time (and committer
                       (date/date-to-iso
                        (.getWhen committer)))
     :commit-name (and author (.getName author))
     :commit-email (and author (.getEmailAddress author))}))

(defn resolve-remote [^String loc]
  (condp #(.startsWith %2 %1) loc
    "https://" loc
    "http://" loc
    "git@" loc
    (io/abspath loc)))

(defn checkout-workspace [clone-home remote workspace org project branch]
  (let [absremote (resolve-remote remote)
        absworkspace (io/abspath-file workspace)
        clonecmd (io/run "git" "clone"
                   "--depth" "2"
                   "--branch" branch
                   absremote absworkspace)
        workspace-repo (git/load-repo absworkspace)
        workspace-ref (git/git-checkout workspace-repo branch false true)
        HEAD (first (git/git-log workspace-repo))]
    (commit-map HEAD)))
