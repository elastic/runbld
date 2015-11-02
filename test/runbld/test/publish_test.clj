(ns runbld.test.publish-test
  (:require [clojure.test :refer :all]
            [elasticsearch.document :as es]
            [elasticsearch.indices :as indices]
            [runbld.process :as proc]
            [runbld.main :as main]
            [runbld.opts :as opts])
  (:require [runbld.publish :as publish] :reload-all))

(deftest ^:integration publish
  (testing "publish to ES"
    (let [opts (opts/parse-args ["-c" "test/runbld.yaml"
                                 "test/success.bash"])
          res (main/run opts)
          q {:query
             {:match
              {:time-start (get-in res [:proc :time-start])}}}]

      (indices/refresh (:es.conn opts) (get-in res [:es :index]))

      (is (= 0 (get-in res [:proc :status])))
      (is (= 0 (-> (es/search
                    (:es.conn opts)
                    {:index (get-in res [:es :index])
                     :type (get-in res [:es :type])
                     :body q})
                   (get-in [:hits :hits])
                   first
                   :_source
                   :status))))))
