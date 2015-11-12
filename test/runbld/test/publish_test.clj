(ns runbld.test.publish-test
  (:require [clojure.test :refer :all]
            [elasticsearch.document :as es]
            [elasticsearch.indices :as indices]
            [runbld.env :as env]
            [runbld.main :as main]
            [runbld.opts :as opts]
            [runbld.process :as proc]
            [schema.test])
  (:require [runbld.vcs.git :as git]
            [runbld.publish :as publish]
            [runbld.publish.elasticsearch :as elasticsearch]
            [runbld.publish.email :as email]
            :reload-all))

(use-fixtures :once schema.test/validate-schemas)

(defn make-error-maker [name]
  (fn [& args]
    (throw
     (Exception. (str name " error!")))))

(deftest handle-errors
  (with-redefs [elasticsearch/index (make-error-maker "es")
                email/send (make-error-maker "email")]
    (let [opts (opts/parse-args ["-c" "test/runbld.yaml"
                                 "--job-name" "elastic,proj1,master"
                                 "test/test.bash"])
          repo (git/init-test-repo (get-in opts [:profiles
                                                 :elastic-proj1-master
                                                 :git :remote]))]
      (is (= 2 (count @(:errors (main/run opts))))))))

(deftest ^:integration elasticsearch
  (with-redefs [publish/handlers (fn []
                                   [#'runbld.publish.elasticsearch/index])]
    (testing "publish to ES"
      (let [opts (opts/parse-args ["-c" "test/runbld.yaml"
                                   "--job-name" "elastic,proj1,master"
                                   "test/success.bash"])
            res (main/run opts)]
        (if (pos? (count @(:errors res)))
          (clojure.pprint/pprint (first @(:errors res)))
          (let [conn (-> opts :es :conn)
                q {:query
                   {:match
                    {:time-start (get-in res [:process :time-start])}}}
                addr (-> (:publish res) :outputs first :output
                         (select-keys [:_index :_type :_id]))
                doc {:index (:_index addr)
                     :type (:_type addr)
                     :id (:_id addr)}]
            (is (= 0 (get-in res [:process :exit-code])))
            (is (= 0 (-> (es/get conn doc)
                         :_source
                         :exit-code)))
            (indices/refresh conn (:index doc))
            (is (= 0 (-> (es/search
                          conn
                          (-> doc
                              (dissoc :id)
                              (assoc :body q)))
                         (get-in [:hits :hits])
                         first
                         :_source
                         :exit-code)))))))))

(deftest email
  (let [sent (atom [])]
    (with-redefs [publish/handlers (fn []
                                     [#'runbld.publish.email/send])
                  email/send* (fn [conn from to subj txt html]
                                (swap! sent conj [to txt
                                                  (apply str (take 10 html))])
                                {:satisfy-schema true})]

      (let [opts (opts/parse-args ["-c" "test/runbld.yaml"
                                   "--job-name" "elastic,proj1,master"
                                   "test/test.bash"])
            repo (git/init-test-repo (get-in opts [:profiles
                                                   :elastic-proj1-master
                                                   :git :remote]))
            res (main/run opts)]
        (when (pos? (count @(:errors res)))
          (println "** errors1 **")
          (clojure.pprint/pprint (first @(:errors res))))
        (is (= 0 (count @(:errors res))))
        (is (= [[["foo@example.com"]
                 "greetings elastic-proj1-master!\n"
                 "<html>\n<bo"]] @sent)))

      (swap! sent pop)

      (let [opts (opts/parse-args ["-c" "test/runbld.yaml"
                                   "--job-name" "elastic,proj2,master"
                                   "test/test.bash"])
            repo (git/init-test-repo (get-in opts [:profiles
                                                   :elastic-proj2-master
                                                   :git :remote]))
            res (main/run opts)]
        (when (pos? (count @(:errors res)))
          (println "** errors2 **")
          (clojure.pprint/pprint (first @(:errors res))))
        (is (= 0 (count @(:errors res))))
        (is (= [[["foo@example.com" "bar@example.com"]
                 "in template for elastic-proj2-master\n"
                 "<html>\n<bo"]] @sent))))))
