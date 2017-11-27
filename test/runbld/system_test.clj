(ns runbld.system-test
  (:require
   [clojure.test :refer :all]
   [robert.bruce :refer [*try*]]
   [runbld.facts.factory :as facter]
   [runbld.fs.factory :as fs]
   [runbld.system :as system]
   [runbld.test-support :as ts]
   [schema.test]))

(use-fixtures :once
  schema.test/validate-schemas
  ts/redirect-logging-fixture)

(deftest basic
  ;; schema does most of the work here, just want to see if it returns
  ;; a map that matches it
  (is (system/inspect-system ".")))

(deftest retries
  (testing "failures retry then bubble"
    (let [tries (atom 0)
          orig-retry system/report-retry]
      (with-redefs [facter/make-facter (fn []
                                         (throw (Exception. "test failure")))
                    system/report-retry (fn [err]
                                          (swap! tries inc)
                                          (orig-retry err))]
        (is (thrown? Exception (system/inspect-system ".")))
        (is (= 5 @tries)))))
  (testing "success after retries"
    (let [tries (atom 0)
          orig-retry system/report-retry
          orig-make-facter facter/make-facter]
      (with-redefs [facter/make-facter (fn []
                                         (if (<= *try* 3)
                                           (throw (Exception. "test failure"))
                                           (orig-make-facter)))
                    system/report-retry (fn [err]
                                          (swap! tries inc)
                                          (orig-retry err))]
        (is (system/inspect-system "."))
        (is (= 3 @tries))))))
