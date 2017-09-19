(ns runbld.util-test
  (:require [clojure.test :refer :all]
            [runbld.util.data :as data]))

(deftest strip-trailing-slashes
  (let [desired "http://example.com/foo/bar"]
    (is (= desired
           (data/strip-trailing-slashes
            "http://example.com/foo/bar")))
    (is (= desired
           (data/strip-trailing-slashes
            "http://example.com/foo/bar////")))
    (is (= desired
           (data/strip-trailing-slashes
            "http://example.com/foo/bar////    ")))
    (is (= desired
           (data/strip-trailing-slashes
            "http://example.com/foo/bar/ ")))))
