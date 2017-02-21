(ns runbld.test.tests-test
  (:require [schema.test])
  (:require [clojure.test :refer :all])
  (:require [runbld.tests :as tests] :reload-all))

(use-fixtures :once schema.test/validate-schemas)

(deftest some-errors
  (testing "java"
    (let [res (tests/capture-failures "test/repo/java/some-errors")]
      (is (= {:errors 1
              :failures 1
              :tests 3
              :skipped 0}
             (select-keys res [:errors :failures :tests :skipped])))
      (is (= (sort ["java.nio.file.NoSuchFileException"
                    "junit.framework.AssertionFailedError"])
             (sort (map :type (:failed-testcases res)))))))
  (testing "python"
    (let [res (tests/capture-failures "test/repo/python/some-errors")]
      (is (= {:errors 1
              :failures 1
              :tests 2
              :skipped 0}
             (select-keys res [:errors :failures :tests :skipped])))
      (is (= (sort ["exceptions.AssertionError"
                    "exceptions.Exception"])
             (sort (map :type (:failed-testcases res))))))))

(deftest no-errors
  (testing "java"
    (let [res (tests/capture-failures "test/repo/java/no-errors")]
      (is (= {:errors 0
              :failures 0
              :tests 1
              :skipped 0}
             (select-keys res [:errors :failures :tests :skipped])))
      (is (= [] (map :type (:failed-testcases res))))))
  (testing "python"
    (let [res (tests/capture-failures "test/repo/python/no-errors")]
      (is (= {:errors 0
              :failures 0
              :tests 1
              :skipped 0}
             (select-keys res [:errors :failures :tests :skipped])))
      (is (= [] (map :type (:failed-testcases res)))))))
