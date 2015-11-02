(ns runbld.test.publish-test
  (:require [clojure.test :refer :all]
            [elasticsearch.document :as es]
            [runbld.process :as proc]
            [runbld.main :as main]
            [runbld.opts :as opts])
  (:require [runbld.publish :as publish] :reload-all))

(deftest ^:integration publish
  (with-redefs []
    (testing "publish to ES"
      (let [opts (opts/parse-args ["-c" "test/runbld.yaml"
                                   "test/success.bash"])
            res (main/run opts)
            es-addr (dissoc (:es res) :body)
            es-res (es/get (:es.conn opts) es-addr)]
        (is (= 0 (get-in res [:proc :status])))
        (is (= {:status 0} (:_source es-res)))))))
