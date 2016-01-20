(ns runbld.test.system-test
  (:require [clojure.test :refer :all]
            [schema.test])
  (:require [runbld.facts.factory :as facter]
            [runbld.fs.factory :as fs]
            [runbld.system :as system]
            :reload-all))

(use-fixtures :once schema.test/validate-schemas)

(deftest basic
  ;; schema does most of the work here, just want to see if it returns
  ;; a map that matches it
  (is (system/inspect-system ".")))
