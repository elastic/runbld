(ns runbld.vcs.git
  (:require
   [clj-git.core :as git]
   [clojure.java.io :as jio]
   [clojure.java.shell :as sh]
   [environ.core :as environ]
   [runbld.io :as io]
   [runbld.schema :refer :all]
   [runbld.util.data :refer [strip-trailing-slashes]]
   [runbld.util.date :as date]
   [runbld.util.debug :as debug]
   [runbld.vcs :as vcs :refer [VcsRepo]]
   [schema.core :as s]
   [slingshot.slingshot :refer [throw+]]))

(def provider "git")

(defn repo? [dir]
  (when dir
    (.isDirectory (jio/file dir ".git"))))

(defn init-test-repo
  ([dir]
   (let [repo (git/git-init dir)]
     (let [basename "this-is-a-test-repo.sh"
           f (jio/file dir basename)]
       (spit f "echo 'this would have been a test build --->here<---'\n")
       (git/git-add repo basename)
       (git/git-commit repo "Add build")
       repo))))

(defn init-test-clone
  ([local-path remote-path]
   (init-test-repo remote-path)
   (git/git-clone local-path remote-path)))

(defmacro with-tmp-repo [bindings & body]
  `(let ~bindings
     (try
       (when (.exists (jio/file ~(bindings 0)))
         (throw (Exception. (format "%s already exists" ~(bindings 0)))))
       (init-test-repo ~(bindings 1))
       ~@body
       (finally
         (io/rmdir-r ~(bindings 0))))))

(defn add-test-commit [dir]
  (let [repo (git/load-repo dir)
        basename (str (java.util.UUID/randomUUID) ".txt")
        f (jio/file dir basename)]
    (spit f "test")
    (git/git-add repo basename)
    (git/git-commit repo (format "Add %s!" basename))))

(defn commit-map [commit]
  (let [message (let [b (-> commit :message :body)]
                  (str (-> commit :message :title)
                       (when (and b (not-empty b)) (str "\n\n" b))))
        commit-time (-> commit :committer :time)
        author-time (-> commit :author :time)]
    (cond-> {:commit-id (:commit commit)
             :commit-short (:commit-short commit)
             :message (-> commit :message :title)
             :message-full message
             :commit-name (-> commit :committer :name)
             :commit-email (-> commit :committer :email)
             :author-name (-> commit :author :name)
             :author-email (-> commit :author :email)
             :provider provider
             :log-pretty (format
                          "commit %s\nAuthor: %s <%s>\nDate:   %s\n\n%s"
                          (:commit commit)
                          (or (-> commit :author :name) "")
                          (or (-> commit :author :email) "")
                          commit-time
                          message)}
      commit-time (assoc :commit-time (str commit-time))
      author-time (assoc :author-time (str author-time)))))

(defn resolve-remote [^String loc]
  (condp #(.startsWith %2 %1) loc
    "https://" loc
    "http://" loc
    "git@" loc
    (str "file://" (io/abspath loc))))

(defn head-commit
  [dir]
  (let [repo (git/load-repo dir)]
    (let [commit (git/git-log-commit repo "HEAD")
          raw-commit (git/git-log-raw-string repo "HEAD")]
      (debug/log "HEAD commit:" commit
                 "Raw string:" raw-commit)
      (if commit
        (commit-map commit)
        (io/log "Failed to get the head-commit. The raw string was:"
                raw-commit)))))

(defn checkout-workspace [clone-home remote workspace org project branch]
  (let [absremote (resolve-remote remote)
        absworkspace (io/abspath-file workspace)
        clonecmd (io/run "git" "clone"
                   "--depth" "2"
                   "--branch" branch
                   absremote absworkspace)
        workspace-repo (git/load-repo absworkspace)
        _ (git/git-checkout workspace-repo branch)
        HEAD (head-commit absworkspace)]
    (commit-map HEAD)))

(defn project-url
  [this]
  (format "https://github.com/%s/%s"
          (.org this)
          (.project this)))

(defn branch-url
  [this]
  (format "%s/tree/%s"
          (project-url this)
          (.branch this)))

(defn commit-url
  [this commit-id]
  (format "%s/commit/%s"
          (project-url this)
          commit-id))

(defn checkout-commit [this commit]
  (git/git-checkout
   (git/load-repo (.dir this)) commit))

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

(defn is-shallow? [this]
  (git/shallow-clone? (git/load-repo (.dir this))))

(defn fetch-latest [this & {:as opts-map}]
  (let [repo (git/load-repo (.dir this))]
    (git/git-fetch
     repo
     (map (fn [[k v]]
            (str "--" (name k) "=" v)) opts-map))
    (git/git-checkout repo "HEAD")))

;; Assume GitHub for now...
(s/defrecord GitRepo
    [dir     :- s/Str  ;; local working copy
     org     :- s/Str
     project :- s/Str
     branch  :- s/Str])

(extend GitRepo
  VcsRepo
  {:log-latest log-latest
   :provider (constantly provider)
   :check-out checkout-commit
   :is-shallow? is-shallow?
   :fetch-latest fetch-latest})

(s/defn make-repo :- GitRepo
  [dir org project branch]
  (GitRepo. dir org project branch))
