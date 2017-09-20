(ns clj-git.core-test
  (:require
   [clj-git.core :as git]
   [clojure.java.io :as io]
   [clojure.test :refer :all]
   [environ.core :as environ]))

(defn abspath [path]
  (-> path
      io/file
      .getCanonicalPath))

(def es-src-dir
  (abspath
   (or (environ/env :elasticsearch-src)
       (-> "clj_git"
           io/resource
           io/file
           .getParent
           (io/file "elasticsearch-src")))))

(defn maybe-prep-repo [dir]
  (if (.exists (io/file dir ".git"))
    (do
      (printf "Elasticsearch source already cloned in %s.  Pulling..." dir)
      (flush)
      (git/git-pull (git/load-repo dir)))
    (do
      (printf "Elasticsearch source missing in %s.  Cloning..." dir)
      (flush)
      (git/git-clone
       dir
       "https://github.com/elastic/elasticsearch.git")))
  (println "  Done."))

(deftest parse-repo-commits
  (maybe-prep-repo es-src-dir)
  (time
   (testing "the first thousand commits can parse"
     (is (= 1000 (->> es-src-dir
                      git/load-repo
                      git/git-log
                      (take 1000)
                      count))))))

(deftest parse-samples
  (doseq [f (->> ["clj_git/commit.txt"
                  "clj_git/commit-no-name.txt"]
                 (map io/resource)
                 (map io/file))]
    (testing f
      (is (-> f slurp git/parse-raw-commit :commit count pos?)))))
