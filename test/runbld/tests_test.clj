(ns runbld.tests-test
  (:require
   [clojure.stacktrace :as stacktrace]
   [clojure.test :refer :all]
   [runbld.test-support :as ts]
   [runbld.tests :as tests]
   [runbld.util.debug :as debug]
   [schema.test]))

(use-fixtures :once schema.test/validate-schemas)

(use-fixtures :each ts/reset-debug-log-fixture)

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
             (sort (map :type (:failed-testcases res)))))))
  (testing "go"
    (let [res (tests/capture-failures "test/repo/go/some-errors")]
      (is (= {:errors 0
              :failures 1
              :tests 1
              :skipped 0}
             (select-keys res [:errors :failures :tests :skipped])))
      (is (= ["Failed"] (map :message (:failed-testcases res)))))))

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
      (is (= [] (map :type (:failed-testcases res))))))
  (testing "go"
    (let [res (tests/capture-failures "test/repo/go/no-errors")]
      (is (= {:errors 0
              :failures 0
              :tests 1
              :skipped 0}
             (select-keys res [:errors :failures :tests :skipped])))
      (is (= [] (map :type (:failed-testcases res)))))))

(deftest empty-file
  (try
    (tests/capture-failures "test/xmls/empty")
    (is :a-ok "This should pass")
    (catch Exception e
      (is false "There shouldn't have been any exceptions")
      (stacktrace/print-cause-trace e))))

(deftest bad-xmls
  (try
    (let [out (with-out-str (tests/capture-failures "test/xmls"))
          log (debug/get-log)]
      (is (re-find #"Failed to parse" out))
      (is (re-find #"(?m)ErrorHandlerWrapper.createSAXParseException"
                   (second log))))
    (catch Exception e
      (is false "There shouldn't have been any exceptions")
      (stacktrace/print-cause-trace e))))
