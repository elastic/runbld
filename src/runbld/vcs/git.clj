(ns runbld.vcs.git
  (:require [clj-jgit.porcelain :as git]
            [clojure.java.io :as jio]
            [environ.core :as environ]
            [runbld.util.date :as date]
            [runbld.util.io :as io]
            [slingshot.slingshot :refer [throw+]]))

(defn init-test-repo [dir]
  (let [basename "this-is-a-test-repo.sh"
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
  (try
    (git/git-fetch-all (git/load-repo dir))
    (catch org.eclipse.jgit.api.errors.InvalidRemoteException e
      (if (environ/env :dev)
        (println "warning: can't fetch origin for" dir)
        (throw+ {:error ::invalid-origin
                 :msg (format "trying to fetch %s" dir)
                 :exception e})))))

(defn clone [remote parent dest]
  (io/mkdir-p parent)
  (git/git-clone2 remote {:path dest :bare false}))

(defn clone-or-fetch [basedir remote org project]
  (let [orgdir (str (jio/file basedir org))
        gitdir (str (jio/file orgdir project))]
    (if (try-repo gitdir)
      (fetch gitdir)
      (clone remote orgdir gitdir))
    gitdir))

(defn commit-map [commit]
  (let [author (.getAuthorIdent commit)]
    {:commit (.getName commit)
     :commit-time (and author (date/date-to-iso
                               (.getWhen author)))
     :author-name (and author (.getName author))
     :author-email (and author (.getEmailAddress author))}))

(defn checkout-workspace [clone-home remote workspace org project branch]
  (io/rmdir-rf workspace)
  (let [cached-remote (clone-or-fetch clone-home remote org project)
        absremote (.getCanonicalPath (jio/file cached-remote))
        clonecmd (git/git-clone2 absremote {:path workspace :bare false
                                     :clone-all-branches true})
        workspace-repo (git/load-repo workspace)
        workspace-ref (git/git-checkout workspace-repo branch false true)
        HEAD (first (git/git-log workspace-repo))]
    (commit-map HEAD)))
