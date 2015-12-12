(ns runbld.test.tests-test
  (:require [schema.test])
  (:require [clojure.test :refer :all])
  (:require [runbld.tests :as tests] :reload-all))

(use-fixtures :once schema.test/validate-schemas)

(deftest basic
  (let [res (tests/capture-failures "test/foo-app")]
    (is (= {:errors 1
            :failures 1
            :tests 3
            :skipped 0}
           (select-keys res [:errors :failures :tests :skipped])))
    (is (= ["java.nio.file.NoSuchFileException"
            "junit.framework.AssertionFailedError"]
           (map :type (:testcases res))))))

(deftest no-failures
  (let [res (tests/capture-failures "src")]
    (is (= {:errors 0
            :failures 0
            :tests 0
            :skipped 0}
           (select-keys res [:errors :failures :tests :skipped])))
    (is (= [] (map :type (:testcases res))))))
