(ns clj-git.core
  (:require [clojure.edn :as edn]
            [clojure.java.shell :as sh]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [instaparse.core :as i])
  (:import (java.time Instant ZoneOffset)))

(defrecord Repository [path])

(defrecord Author [name email time])

(defrecord Committer [name email time])

(defrecord Message [title body])

(defrecord Commit [commit tree parent author committer message])

(defn abspath [f]
  (.getCanonicalPath (io/as-file f)))

(defn abspath-file [f]
  (io/file (abspath f)))

(defn parent-dir [dir]
  (-> dir abspath-file .toPath .getParent str))

(defn run
  ([cmd args]
   (run cmd args "."))
  ([cmd args dir]
   #_(prn cmd args dir)
   ;; sh/sh will eventually call clojure.java.shell/parse-args which
   ;; will `split-with string?` and will assume the second group is
   ;; a map.  Long story short, make sure that all of our args are
   ;; strings.
   (let [args (map str args)
         res (apply sh/sh "git" cmd (concat args [:dir dir]))
         err (fn [r]
               (assert
                (= 0 (:exit r)) (format "%s: \nout: %s\nerr: %s"
                                        (pr-str [cmd args])
                                        (:out r) (:err r))))
         exit (:exit res)]
     (if (pos? exit)
       (case exit
         128 (if (.contains (:err res) "bad object") nil (err res))
         (err res))
       res))))

(defn git
  ([repo cmd]
   (git repo cmd []))
  ([repo cmd args]
   (run cmd args (:path repo))))

(def fuller
  (io/resource "clj_git/grammar/fuller.bnf"))

(def raw
  (io/resource "clj_git/grammar/raw.bnf"))

(def parse-log-fuller
  (i/parser fuller))

(def parse-log-raw
  (i/parser raw))

(def log-fuller
  ["-1" "--date=iso-strict" "--pretty=fuller"])

(def raw-transforms
  {:git
   (fn root [& x]
     (let [{:keys [commit parent author committer
                   title body time tree] :as m} (apply merge x)
           c (map->Commit
              {:commit commit
               :tree tree
               :parent parent
               :author author
               :committer committer
               :message (->Message title body)
               :commit-short (->> commit (take 7) (apply str))})]
       c
       ))

   :name
   #(.trim %)

   :commitLine
   (fn [[_ sha]]
     {:commit sha})

   :treeLine
   (fn [[_ sha]]
     {:tree sha})

   :parentLine
   (fn [[_ sha]]
     {:parent sha})

   :authorLine
   (fn
     ([[_ email] time]
      {:author
       (->Author "" email time)})
     ([name [_ email] time]
      {:author
       (->Author name email time)}))

   :committerLine
   (fn
     ([[_ email] time]
      {:committer
       (->Committer "" email time)})
     ([name [_ email] time]
      {:committer
       (->Committer name email time)}))

   :msgTitle
   (fn [& x]
     {:title (->> x (apply str) .trim)})

   :msgBody
   (fn [& x]
     {:body (->> x (apply str) .trim)})

   :gpgsig
   (fn [& x]
     {:gpgsig (apply str x)})

   :gpgPrefix
   (fn [x] x)

   :gpgPostfix
   (fn [x] x)

   :gpgHeader
   (fn [& x]
     (apply str x))

   :gpgArmorHeaderKey
   (fn [x] x)

   :gpgData
   (fn [& x]
     (apply str x))

   :newline
   (fn [x] x)

   :nonNewline
   (fn [x] x)

   :base64
   (fn [[x]] x)

   :time
   (fn [[_ epoch-str] [_ sign hours]]
     (.toInstant
      (.atOffset
       (Instant/ofEpochSecond (edn/read-string epoch-str))
       (ZoneOffset/of (str sign hours)))))})

(defn git-log-raw-string
  ([repo]
   (git-log-raw-string repo "HEAD"))
  ([repo sha]
   (:out (git repo "log" [sha "-1" "--pretty=raw"]))))

(defn parse-raw-commit [s]
  (let [res (i/transform raw-transforms (parse-log-raw s))]
    (if (instance? instaparse.gll.Failure res)
      (throw (ex-info "parse failure" {:res res
                                       :commit s}))
      res)))

(defn git-log-commit
  ([repo]
   (git-log-commit repo "HEAD"))
  ([repo sha]
   (when-let [raw (git-log-raw-string repo sha)]
     (parse-raw-commit raw))))

(defn git-log
  ([repo]
   (git-log repo "HEAD"))
  ([repo sha]
   (when-let [commit (git-log-commit repo sha)]
     (lazy-seq
      (cons commit (when-let [p (:parent commit)]
                     (git-log repo p)))))))

(defn load-repo [path]
  (if (instance? Repository path)
    path
    (->Repository path)))

(defn git-init [repo]
  (let [repo (load-repo repo)]
    (.mkdirs (io/file (:path repo)))
    (git repo "init")
    repo))

(defn git-add [repo basename]
  (git repo "add" [basename]))

(defn git-commit [repo msg]
  (git repo "commit" ["-m" msg]))

(defn git-checkout [repo sha-ish]
  (git repo "checkout" ["-f" sha-ish]))

(defn git-branch [repo]
  (-> (git repo "rev-parse" ["--abbrev-ref" "HEAD"])
      :out
      str/trim-newline))

(defn git-clone
  "Clones a git repository.  `local' is either a path that is fully
  qualified or relative to the current working directory that
  specifies to where the `remote' repository will be cloned.  `remote'
  is a standard Git URL.

  The following two are equivalent:
  clj: (git-clone \"/home/user/src/elasticsearch-source\"
        \"git@github.com:elastic/elasticsearch.git\")

  cli: git clone git@github.com:elastic/elasticsearch.git \\
         /home/user/src/elasticsearch-source"
  ([local remote]
   (git-clone local remote []))
  ([local remote args]
   (.mkdirs (io/file (parent-dir local)))
   ;; don't worry about local/remote & hardlinks for now
   (run "clone" (concat [remote local] args))))

(defn git-pull
  "Runs `git pull' on the provided repo.  `args' is a vector of
  options as they would be specified on the command line.

  clj: (git-pull repo [\"--rebase\"])
  cli: git pull --rebase"
  ([repo]
   (git-pull repo []))
  ([repo args]
   (git repo "pull" args)))

(defn git-fetch
  "Runs `git fetch' on the provided repo.  `args' is a vector of
  options as they would be specified on the command line.

  clj: (git-fetch repo [\"--all\"])
  cli: git fetch --all"
  ([repo]
   (git-fetch repo []))
  ([repo args]
   (git repo "fetch" args)))

(defn git-remote
  "Runs `git remote' on the provided repo.  `args' is a vector of
  options as they would be specified on the command line."
  [repo args]
  (git repo "remote" args))

(defn shallow-clone?
  "Returns true if the repo is shallow, false otherwise."
  [repo]
  (.exists (io/file (:path repo) ".git" "shallow")))
