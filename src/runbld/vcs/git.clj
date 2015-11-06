(ns runbld.vcs.git
  (:require [clj-jgit.porcelain :as git]
            [clojure.java.io :as jio]
            [runbld.util.io :as io]))

(defn init-test-repo [dir]
  (let [basename "build.sh"
        f (jio/file dir basename)
        repo (git/git-init dir)]
    (spit f "echo building... done")
    (git/git-add repo basename)
    (git/git-commit repo "Add build")
    repo))

(defn add-test-commit [dir]
  (let [repo (git/load-repo dir)
        basename (str (java.util.UUID/randomUUID) ".txt")
        f (jio/file dir basename)]
    (spit f "test")
    (git/git-add repo basename)
    (git/git-commit repo (format "Add %s!" basename))))

(defn try-repo [dir]
  (try
    (git/load-repo dir)
    (catch java.io.FileNotFoundException _)))

(defn fetch [dir]
  (git/git-fetch-all (git/load-repo dir)))

(defn clone [remote parent dest]
  (io/mkdir-p parent)
  (git/git-clone remote dest))

(defn clone-or-fetch [basedir remote org project]
  (let [orgdir (jio/file basedir org)
        gitdir (jio/file orgdir project)]
    (if (try-repo gitdir)
      (fetch gitdir)
      (clone remote orgdir gitdir))))
