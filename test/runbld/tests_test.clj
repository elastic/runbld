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

(def filename-pattern "/TEST-.*\\.xml$")

(deftest some-errors
  (testing "java"
    (let [res (tests/capture-failures "test/repo/java/some-errors"
                                      filename-pattern)]
      (is (= {:errors 1
              :failures 1
              :tests 3
              :skipped 0}
             (select-keys res [:errors :failures :tests :skipped])))
      (is (= (sort ["java.nio.file.NoSuchFileException"
                    "junit.framework.AssertionFailedError"])
             (sort (map :type (:failed-testcases res)))))))
  (testing "python"
    (let [res (tests/capture-failures "test/repo/python/some-errors"
                                      filename-pattern)]
      (is (= {:errors 1
              :failures 1
              :tests 2
              :skipped 0}
             (select-keys res [:errors :failures :tests :skipped])))
      (is (= (sort ["exceptions.AssertionError"
                    "exceptions.Exception"])
             (sort (map :type (:failed-testcases res)))))))
  (testing "go"
    (let [res (tests/capture-failures "test/repo/go/some-errors"
                                      filename-pattern)]
      (is (= {:errors 0
              :failures 1
              :tests 1
              :skipped 0}
             (select-keys res [:errors :failures :tests :skipped])))
      (is (= ["Failed"] (map :message (:failed-testcases res)))))))

(deftest no-errors
  (testing "java"
    (let [res (tests/capture-failures "test/repo/java/no-errors"
                                      filename-pattern)]
      (is (= {:errors 0
              :failures 0
              :tests 1
              :skipped 0}
             (select-keys res [:errors :failures :tests :skipped])))
      (is (= [] (map :type (:failed-testcases res))))))
  (testing "python"
    (let [res (tests/capture-failures "test/repo/python/no-errors"
                                      filename-pattern)]
      (is (= {:errors 0
              :failures 0
              :tests 1
              :skipped 0}
             (select-keys res [:errors :failures :tests :skipped])))
      (is (= [] (map :type (:failed-testcases res))))))
  (testing "go"
    (let [res (tests/capture-failures "test/repo/go/no-errors"
                                      filename-pattern)]
      (is (= {:errors 0
              :failures 0
              :tests 1
              :skipped 0}
             (select-keys res [:errors :failures :tests :skipped])))
      (is (= [] (map :type (:failed-testcases res)))))))

(deftest empty-file
  (try
    (tests/capture-failures "test/xmls/empty" filename-pattern)
    (is :a-ok "This should pass")
    (catch Exception e
      (is false "There shouldn't have been any exceptions")
      (stacktrace/print-cause-trace e))))

(deftest bad-xmls
  (try
    (let [out (with-out-str (tests/capture-failures "test/xmls"
                                                    filename-pattern))
          log (debug/get-log)]
      (is (re-find #"Failed to parse" out))
      (is (some #(re-find #"(?m)ErrorHandlerWrapper.createSAXParseException" %)
                log)))
    (catch Exception e
      (is false "There shouldn't have been any exceptions")
      (stacktrace/print-cause-trace e))))

(deftest junit-filename-pattern
  (let [res (tests/capture-failures "test"
                                    "/(some-errors|more-errors)\\.xml$")]
    (is (= {:errors 2
            :failures 2
            :tests 6
            :skipped 0}
           (select-keys res [:errors :failures :tests :skipped])))
    (is (= (set ["java.nio.file.NoSuchFileException"
                 "junit.framework.AssertionFailedError"])
           (set (map :type (:failed-testcases res))))))
  (let [res (tests/capture-failures "test"
                                    "/more-errors\\.xml$")]
    (is (= {:errors 1
            :failures 1
            :tests 3
            :skipped 0}
           (select-keys res [:errors :failures :tests :skipped])))
    (is (= (set ["java.nio.file.NoSuchFileException"
                 "junit.framework.AssertionFailedError"])
           (set (map :type (:failed-testcases res)))))))
