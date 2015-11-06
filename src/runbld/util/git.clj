(ns runbld.util.git
  (:require [clj-jgit.porcelain :as git]
            [clojure.java.io :as io]))

(defn make-repo [dir]
  (git/git-init dir))

(defn make-tmp-repo [dir]
  (let [basename "build.sh"
        f (io/file dir basename)
        repo (make-repo dir)]
    (spit f "echo building... done")
    (git/git-add repo basename)
    (git/git-commit repo "Add build")
    repo))

(defn delete-repo [dir]
  (let [pb (ProcessBuilder. ["rm" "-r" dir])
        p (.start pb)]
    (assert (= 0 (.waitFor p))
            (-> p .getErrorStream io/reader slurp .trim))))
