(ns runbld.test.scm-test
  (:require [clojure.test :refer :all]
            [environ.core :as environ]
            [runbld.scm :refer :all]))

(defn test-choose-branch [env-branch scm-branch job-branch]
  (with-redefs [environ/env {:branch-specifier env-branch}]
    (choose-branch {:scm {:branch scm-branch}
                    :build {:branch job-branch}})))

(deftest branch-selection
  {:scm {:branch "1.6"}
   :build {:branch "3.2"}}
  (testing "Branch selection chooses the right option."
    (is (= {:branch "1.6" :commit nil}
           (test-choose-branch nil "1.6" "3.2"))
        "env is nil, should be ignored")
    (is (= {:branch "1.6" :commit nil}
           (test-choose-branch "" "1.6" "3.2"))
        "env is '', should be ignored")
    (is (= {:branch "1.6" :commit nil}
           (test-choose-branch "refs/heads/3.2" "1.6" "3.2"))
        "env and build are the 'same', should choose scm")
    (is (= {:branch "6.4" :commit nil}
           (test-choose-branch "6.4" "1.6" "3.2"))
        "env and build are different, should choose env"))
  (testing "Branch selection can handle commits."
    (is (= {:commit "026aac0" :branch "1.6"}
           (test-choose-branch "026aac0" "1.6" "3.2"))
        "Commits are supported")
    (is (= {:commit "026aac0" :branch "3.2"}
           (test-choose-branch nil "026aac0" "3.2"))
        "Commits are supported, even when not in the env")
    (is (= {:commit "abc1234" :branch "3.2"}
           (test-choose-branch "abc1234" "026aac0" "3.2"))
        "The first of multiple commits will be chosen.")
    ;; I don't know what would lead to this, but if its commits all
    ;; the way down, choose the first and assume we're working with
    ;; master
    (is (= {:commit "abc1234" :branch "master"}
           (test-choose-branch "abc1234" "026aac0" "def5678"))
        "Use master if no branches are found")
    (testing "A commit is ignored if it's preceded by a branch"
      (is (= {:branch "6.4" :commit nil}
             (test-choose-branch "6.4" "026aac0" "3.2")))
      (is (= {:branch "1.6" :commit nil}
             (test-choose-branch nil "1.6" "026aac0"))))))
