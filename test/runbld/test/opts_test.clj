(ns runbld.test.opts-test
  (:require [clojure.test :refer :all])
  (:require [runbld.opts :as opts] :reload-all))

(deftest opts
  (testing "config file"
    (is (= {:es.url "http://localhost:9200",
            :es.index.build "FOO",
            :es.index.config "runbld",
            :from.the.config.file "this",
            :config "test/runbld.yaml",
            :scriptfile "/path/to/script.bash"}
           (dissoc (opts/parse-args ["-c" "test/runbld.yaml"
                                     "--es.index.build" "FOO"
                                     "/path/to/script.bash"])
                   :es.conn)))))
