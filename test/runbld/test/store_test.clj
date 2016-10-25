(ns runbld.test.store-test
  (:require clojure.spec.test
            [clojure.test :refer :all]
            [runbld.store :as store]))

(clojure.spec.test/instrument)

(def ES "http://localhost:9200")

(deftest t
  (let [conn (store/make-connection
              {:elasticsearch.connection.http/url ES
               :elasticsearch.connection.http/http-opts {}})]
    (is (= :foo (store/begin-process! :foo)))))
