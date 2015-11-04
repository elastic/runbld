(ns runbld.test.publish-test
  (:require [clojure.test :refer :all]
            [elasticsearch.document :as es]
            [elasticsearch.indices :as indices]
            [runbld.env :as env]
            [runbld.main :as main]
            [runbld.opts :as opts]
            [runbld.process :as proc])
  (:require [runbld.publish :as publish]
            [runbld.publish.elasticsearch :as elasticsearch]
            [runbld.publish.email :as email]
            :reload-all))

(defn make-error-maker [name]
  (fn [& args]
    (throw
     (Exception. (str name " error!")))))

(deftest handle-errors
  (with-redefs [elasticsearch/index (make-error-maker "es")
                email/send (make-error-maker "email")]
    (let [opts (opts/parse-args ["-c" "test/runbld.yaml" "test/success.bash"])]
      (is (= 2 (count @(:errors (main/run opts))))))))

(deftest ^:integration elasticsearch
  (with-redefs [publish/handlers (fn []
                                   [#'runbld.publish.elasticsearch/index])]
    (testing "publish to ES"
      (let [opts (opts/parse-args ["-c" "test/runbld.yaml"
                                   "test/success.bash"])
            res (main/run opts)
            q {:query
               {:match
                {:time-start (get-in res [:proc :time-start])}}}
            addr (-> (:publish res) :outputs first :output
                     (select-keys [:_index :_type :_id]))
            doc {:index (:_index addr)
                 :type (:_type addr)
                 :id (:_id addr)}]
        (is (= 0 (get-in res [:proc :status])))
        (is (= 0 (-> (es/get (:es.conn opts) doc)
                     :_source
                     :status)))
        (indices/refresh (:es.conn opts) (:index doc))
        (is (= 0 (-> (es/search
                      (:es.conn opts)
                      (-> doc
                          (dissoc :id)
                          (assoc :body q)))
                     (get-in [:hits :hits])
                     first
                     :_source
                     :status)))))))

(deftest email
  (let [sent (atom [])]
    (with-redefs [email/send* (fn [conn from to subj body]
                                (swap! sent conj body))
                  env/facter (fn [& args] {:some :fact})]
      (let [opts (opts/parse-args ["-c" "test/runbld.yaml"
                                   "test/success.bash"])
            res (main/run opts)]
        (is (= @sent ["greetings elastic-dev-master!\n"]))))))
