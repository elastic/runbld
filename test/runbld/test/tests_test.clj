(ns runbld.test.tests-test
  (:require [schema.test])
  (:require [clojure.test :refer :all])
  (:require [runbld.tests :as tests] :reload-all))

(use-fixtures :once schema.test/validate-schemas)

(deftest some-errors
  (let [res (tests/capture-failures "test/repo/some-errors")]
    (is (= {:errors 1
            :failures 1
            :tests 3
            :skipped 0}
           (select-keys res [:errors :failures :tests :skipped])))
    (is (= ["java.nio.file.NoSuchFileException"
            "junit.framework.AssertionFailedError"]
           (map :type (:failed-testcases res))))))

(deftest no-errors
  (let [res (tests/capture-failures "test/repo/no-errors")]
    (is (= {:errors 0
            :failures 0
            :tests 1
            :skipped 0}
           (select-keys res [:errors :failures :tests :skipped])))
    (is (= [] (map :type (:failed-testcases res))))))
