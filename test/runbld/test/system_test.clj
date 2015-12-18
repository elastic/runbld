(ns runbld.test.system-test
  (:require [clojure.test :refer :all]
            [schema.test])
  (:require [runbld.system :as system]
            :reload-all))

(use-fixtures :once schema.test/validate-schemas)

(deftest number-inconsistencies
  (is (= 1 (system/as-int "1")))
  (is (= 1 (system/as-int 1)))
  (is (nil? (system/as-int nil))))
