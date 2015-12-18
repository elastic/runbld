(ns runbld.vcs.git
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [clj-jgit.porcelain :as git]
            [clojure.java.io :as jio]
            [clojure.java.shell :as sh]
            [environ.core :as environ]
            [runbld.util.data :refer [strip-trailing-slashes]]
            [runbld.util.date :as date]
            [runbld.util.io :as io]
            [runbld.vcs :as vcs :refer [VcsRepo]]
            [slingshot.slingshot :refer [throw+]]))

(def vendor "git")

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

(defmacro with-tmp-repo [bindings & body]
  `(let ~bindings
     (try
       (when (.exists (jio/file ~(bindings 0)))
         (throw (Exception. (format "%s already exists" ~(bindings 0)))))
       (init-test-repo ~(bindings 1))
       ~@body
       (finally
         (io/rmdir-rf ~(bindings 0))))))

(defn add-test-commit [dir]
  (let [repo (git/load-repo dir)
        basename (str (java.util.UUID/randomUUID) ".txt")
        f (jio/file dir basename)]
    (spit f "test")
    (git/git-add repo basename)
    (git/git-commit repo (format "Add %s!" basename))))

(defn commit-map [commit]
  (let [author (.getAuthorIdent commit)
        committer (.getCommitterIdent commit)
        commit-id (.getName commit)
        author-name (and author (.getName author))
        author-email (and author (.getEmailAddress author))
        message (.getShortMessage commit)
        commit-time (and committer
                         (date/date-to-iso
                          (.getWhen committer)))]
    {:commit-id commit-id
     :commit-short (->> commit-id
                        (take 7)
                        (apply str))

     :message message
     :message-full (.getFullMessage commit)
     :commit-time commit-time
     :commit-name (and committer (.getName committer))
     :commit-email (and committer (.getEmailAddress committer))
     :author-time (and author
                       (date/date-to-iso
                        (.getWhen author)))
     :author-name author-name
     :author-email author-email
     :type vendor
     :log-pretty (format
                  "commit %s\nAuthor: %s <%s>\nDate:   %s\n\n%s"
                  commit-id
                  (or author-name "")
                  (or author-email "")
                  commit-time
                  message)}))

(defn resolve-remote [^String loc]
  (condp #(.startsWith %2 %1) loc
    "https://" loc
    "http://" loc
    "git@" loc
    (str "file://" (io/abspath loc))))

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

(defn head-commit
  [dir]
  (let [HEAD (first
              (git/git-log
               (git/load-repo dir)))]
    (commit-map HEAD)))

(defn branch-url
  [this]
  (if (and (.url this)
           (.contains (.url this) "github.com"))
    (format "%s/tree/%s"
            (strip-trailing-slashes (.url this))
            (.branch this))))

(defn commit-url
  [this commit-id]
  (if (and (.url this)
           (.contains (.url this) "github.com"))
    (format "%s/commit/%s"
            (strip-trailing-slashes (.url this))
            commit-id)))

(defn project-url
  [this]
  (.url this))

(s/defn log-latest :- VcsLog
  ([this]
   (let [{:keys [commit-id] :as commit} (head-commit (.dir this))]
     (merge
      commit
      {:project-url (project-url this)}
      (when-let [u (branch-url this)]
        {:branch-url u})
      (when-let [u (commit-url this commit-id)]
        {:commit-url u})))))

(s/defrecord GitRepo
    [dir     :- s/Str  ;; local working copy
     url     :- s/Str  ;; remote
     org     :- s/Str
     project :- s/Str
     branch  :- s/Str])

(extend GitRepo
  VcsRepo
  {:log-latest log-latest
   :vendor (fn [& args] vendor)})

(s/defn make-repo :- GitRepo
  [dir url org project branch]
  (GitRepo. dir url org project branch))
