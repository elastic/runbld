(ns clj-git.test.core-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all])
  (:require [clj-git.core :as git] :reload-all))

(def es-src-dir (-> "clj_git"
                    io/resource
                    io/file
                    .getParent
                    (io/file "elasticsearch-src")
                    .getCanonicalPath))

(defn maybe-prep-repo [dir]
  (if (.exists (io/file dir ".git"))
    (do
      (print "Elasticsearch source already cloned.  Pulling...")
      (flush)
      (git/git-pull (git/load-repo dir)))
    (do
      (print "Elasticsearch source missing.  Cloning...")
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
