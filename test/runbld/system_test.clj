(ns runbld.system-test
  (:require
   [clojure.test :refer :all]
   [robert.bruce :refer [*try*]]
   [runbld.facts.factory :as facter]
   [runbld.system :as system]
   [runbld.test-support :as ts]
   [schema.test]))

(def log-lines (atom []))

(use-fixtures :once
  schema.test/validate-schemas)

(use-fixtures :each
  (ts/redirect-logging-fixture log-lines))

(deftest retries
  (testing "failures retry then bubble"
    (let [tries (atom 0)
          orig-retry system/report-retry]
      (with-redefs [facter/make-facter (fn []
                                         (assert nil "test failure"))
                    system/report-retry (fn [err]
                                          (swap! tries inc)
                                          (orig-retry err))]
        (is (thrown-with-msg? AssertionError #"test failure"
                              (system/inspect-system ".")))
        (is (= 5 @tries)))))
  (testing "success after retries"
    (let [tries (atom 0)
          orig-retry system/report-retry
          orig-make-facter facter/make-facter]
      (reset! log-lines [])
      (with-redefs [facter/make-facter (fn []
                                         (if (<= *try* 3)
                                           (throw (Exception.
                                                   (str "test failure " *try*)))
                                           (orig-make-facter)))
                    system/report-retry (fn [err]
                                          (swap! tries inc)
                                          (orig-retry err))]
        (is (system/inspect-system "."))
        (is (= 3 @tries))
        (is (= 3 (count (filter #(re-find #"test failure \d" %)
                                @log-lines))))))))
