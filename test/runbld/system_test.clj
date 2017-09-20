(ns runbld.system-test
  (:require
   [clojure.test :refer :all]
   [runbld.facts.factory :as facter]
   [runbld.fs.factory :as fs]
   [runbld.system :as system]
   [schema.test]))

(use-fixtures :once schema.test/validate-schemas)

(deftest basic
  ;; schema does most of the work here, just want to see if it returns
  ;; a map that matches it
  (is (system/inspect-system ".")))
