(ns runbld.test.post-test
  (:require [clojure.test :refer :all]
            [runbld.post.test :as t]))

(deftest failures
  (is (= 1 (count
            (t/capture-failures "test/test-reports")))))
