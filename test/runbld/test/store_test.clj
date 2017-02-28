(ns runbld.test.store-test
  (:require [clojure.spec :as s]
            [clojure.spec.test :as st]
            [clojure.test :refer :all]
            [elasticsearch.indices :as indices]
            [runbld.store :as store]))

(clojure.spec.test/instrument)

(defonce conn
  (store/make-connection {:url "http://localhost:9200" :http-opts {}}))

(deftest choose-index
  (let [pre "runbld-test"
        settings {:settings
                  {:index
                   {:number_of_shards 1
                    :number_of_replicas 0}}}
        idx1 (store/create-timestamped-index conn pre settings)
        idx2 (store/create-timestamped-index conn pre settings)
        target-index (store/set-up-index conn pre settings)]
    (is (= target-index idx2))
    (indices/delete conn idx2)
    (indices/delete conn idx1)))

#_(deftest run-test-check-tests
    #_(testing "That the auto-generated quick-check style tests pass."
        (let [summary (st/summarize-results
                       (st/check
                        (st/enumerate-namespace 'runbld.store)
                        {:clojure.spec.test.check/opts {:num-tests 1}}))]
          (is (= (:total summary)
                 (:check-passed summary)))))
    (testing "That the auto-generated quick-check style tests pass."
      (let [summary (st/summarize-results
                     (st/check
                      (st/enumerate-namespace 'runbld.store)
                      {:clojure.spec.test.check/opts {:num-tests 1}}))]
        (is (= (:total summary)
               (:check-passed summary))))))
