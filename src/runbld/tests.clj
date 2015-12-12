(ns runbld.tests
  (:require [runbld.tests.junit]))

(defn capture-failures [workspace]
  (runbld.tests.junit/find-failures workspace))
