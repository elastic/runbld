(ns runbld.test.process-test
  (:require [clojure.test :refer :all])
  (:require [runbld.process :as proc] :reload-all))

(deftest process
  (testing "success"
    (is (= 0 (:status (proc/run "test/success.bash")))))
  (testing "success"
    (is (= 1 (:status (proc/run "test/fail.bash"))))))
