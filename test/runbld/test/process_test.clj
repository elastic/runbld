(ns runbld.test.process-test
  (:require [clojure.test :refer :all])
  (:require [runbld.process :as proc] :reload-all))

(deftest process
  (testing "success"
    (let [res (proc/exec "bash" "-x" "test/success.bash" "tmp")]
      (is res)
      (is (= 0 (:exit-code res)))))
  (testing "fail"
    (let [res (proc/exec "bash" "-x" "test/fail.bash" "tmp")]
      (is res)
      (is (= 1 (:exit-code res))))))

(deftest output-io
  (with-open [out (java.io.PrintWriter. "tmp/.master.log")]
    (let [res (binding [*out* out]
                (proc/exec "bash" "-e" "test/output.bash" "tmp"))]
      (testing "master process stdout"
        (is (= (:out-bytes res) (count
                                 (slurp "tmp/.master.log"))))
        (is (= 10 (count
                   (line-seq
                    (clojure.java.io/reader
                     "tmp/.master.log"))))))
      (testing "stdout"
        (is (= (:out-bytes res) (count
                                 (slurp "tmp/.stdout.log"))))
        (is (= 10 (count
                   (line-seq
                    (clojure.java.io/reader
                     "tmp/.stdout.log"))))))
      (testing "stderr"
        (is (= (:err-bytes res) (count
                                 (slurp "tmp/.stderr.log"))))
        (is (= 1 (count
                  (line-seq
                   (clojure.java.io/reader
                    "tmp/.stderr.log")))))))))
