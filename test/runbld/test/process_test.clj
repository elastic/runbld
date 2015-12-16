(ns runbld.test.process-test
  (:require [clojure.test :refer :all]
            [schema.test])
  (:require [runbld.process :as proc]
            [runbld.util.data :as data] :reload-all))

(use-fixtures :once schema.test/validate-schemas)

(deftest process
  (testing "success"
    (let [err (java.io.StringWriter.)]
      (binding [*out* (java.io.StringWriter.)
                *err* err]
        (let [res (proc/exec "bash" "-x" "test/success.bash" "tmp")]
          (is res)
          (is (= 0 (:exit-code res)))
          (is (= "+ exit 0\n" (str err)))))))
  (testing "fail"
    (let [err (java.io.StringWriter.)]
      (binding [*out* (java.io.StringWriter.)
                *err* err]
        (let [res (proc/exec "bash" "-x" "test/fail.bash" "tmp")]
          (is res)
          (is (= 1 (:exit-code res)))
          (is (= "+ exit 1\n" (str err))))))))

(deftest output-io
  (with-open [out (java.io.PrintWriter. "tmp/.master.log")]
    (let [res (binding [*out* out
                        *err* (java.io.StringWriter.)]
                (proc/exec "bash" "-e" "test/output.bash" "tmp"))]
      (testing "master process stdout"
        (is (= (bigdec 100)
               (data/unscaled-percent
                (:out-accuracy res))))
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
