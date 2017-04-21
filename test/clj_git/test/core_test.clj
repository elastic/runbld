(ns clj-git.test.core-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all])
  (:require [clj-git.core :as git] :reload-all))

(deftest parse-commit
  (time
   (testing "the first thousand commits can parse"
     (is (= 1000 (->> (io/file
                       (System/getProperty "user.home")
                       "src" "elastic" "elasticsearch")
                      str
                      git/load-repo
                      git/git-log
                      (take 1000)
                      count))))))
