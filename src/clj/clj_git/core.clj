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
   (prn cmd args dir)
   (let [res (apply sh/sh "git" cmd (concat args [:dir dir]))]
     (assert (= 0 (:exit res)) (format "%s: \nout: %s\nerr: %s"
                                       (pr-str [cmd args])
                                       (:out res)
                                       (:err res)))
     res)))

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
   (fn root
     ([commit tree author committer title body]
      (root commit tree nil author committer title body))
     ([commit tree parent author committer title body]
      (map->Commit
       (merge commit tree parent
              {:author author
               :committer committer
               :message (map->Message (merge title body))
               :commit-short (->> (:commit commit)
                                  (take 7)
                                  (apply str))}))))

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
   (fn [name [_ email] time]
     (->Author name email time))

   :committerLine
   (fn [name [_ email] time]
     (->Committer name email time))

   :msgTitle
   (fn [& x]
     {:title (->> x (map second) (apply str))})

   :msgBody
   (fn [& x]
     {:body (->> x (map second) (apply str) .trim)})

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
  (i/transform raw-transforms (parse-log-raw s)))

(defn git-log-commit
  ([repo]
   (git-log-commit repo "HEAD"))
  ([repo sha]
   (parse-raw-commit (git-log-raw-string repo sha))))

(defn git-log
  ([repo]
   (git-log repo "HEAD"))
  ([repo sha]
   (let [commit (git-log-commit repo sha)]
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

(defn git-clone [local remote]
  (.mkdirs (io/file (parent-dir local)))
  ;; don't worry about local/remote & hardlinks for now
  (run "clone" [remote local]))
