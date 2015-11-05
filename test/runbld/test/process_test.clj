(ns runbld.test.process-test
  (:require [clojure.test :refer :all])
  (:require [runbld.process :as proc] :reload-all))

(deftest process
  (testing "success"
    (let [res (proc/exec "bash" "-x" "test/success.bash")]
      (is res)
      (is (= 0 (:status res)))))
  (testing "fail"
    (let [res (proc/exec "bash" "-x" "test/fail.bash")]
      (is res)
      (is (= 1 (:status res))))))
