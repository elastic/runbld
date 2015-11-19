(ns runbld.post.test
  (:require [runbld.post.test.junit]))

(defn capture-failures [workspace]
  (runbld.post.test.junit/find-failures workspace))
