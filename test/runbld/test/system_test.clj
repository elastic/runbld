(ns runbld.test.system-test
  (:require [clojure.test :refer :all]
            [schema.test])
  (:require [runbld.system :as system]
            :reload-all))

(use-fixtures :once schema.test/validate-schemas)

(deftest facter
  (let [facts (system/inspect-system
               (system/make-facter))]
    (is (:os facts))))
