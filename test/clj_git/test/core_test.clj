(ns clj-git.test.core-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [environ.core :as environ])
  (:require [clj-git.core :as git] :reload-all))

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

(deftest parse-commit
  (maybe-prep-repo es-src-dir)
  (time
   (testing "the first thousand commits can parse"
     (is (= 1000 (->> es-src-dir
                      git/load-repo
                      git/git-log
                      (take 1000)
                      count))))))
