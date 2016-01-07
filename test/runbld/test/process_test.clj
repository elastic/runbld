(ns runbld.test.process-test
  (:require [clojure.test :refer :all]
            [schema.test])
  (:require [runbld.java :as java]
            [runbld.opts :as opts]
            [runbld.process :as proc]
            [runbld.util.data :as data]
            [runbld.util.io :as io] :reload-all))

(use-fixtures :once schema.test/validate-schemas)

(deftest output-io
  (io/with-tmp-dir [dir ["tmp" (str *ns* "-")]]
    (let [master (io/file dir "master.log")
          stdout (io/file dir ".stdout.log")
          stderr (io/file dir ".stderr.log")]
      (with-open [out (java.io.PrintWriter. master)]
        (let [res (binding [*out* out
                            *err* (java.io.StringWriter.)]
                    (let [args ["-c" "test/config/main.yml"
                                "-j" "elastic+foo+master"
                                "-d" (str dir)
                                "test/output.bash"]
                          opts (opts/parse-args args)]
                      ((-> #'proc/output-capturing-run
                           java/wrap-java)
                       (select-keys opts [:process :es :java]))))]
          ;; (println "ls -l" (str dir))
          ;; (println (:out (io/run "ls" "-la" (str dir))))
          ;; (println "master:")
          ;; (println (slurp master))
          ;; (println "stdout:")
          ;; (println (slurp stdout))
          ;; (println "stderr:")
          ;; (prn (slurp stderr))

          (testing "master process stdout"
            (is (= (bigdec 100)
                   (data/unscaled-percent
                    (:out-accuracy res))))
            (is (= (:out-bytes res) (count (slurp master))))
            (is (= 10 (count
                       (line-seq
                        (clojure.java.io/reader master))))))
          (testing "stdout"
            (is (= (:out-bytes res) (count (slurp stdout))))
            (is (= 10 (count
                       (line-seq
                        (clojure.java.io/reader stdout))))))
          (testing "stderr"
            (is (= (:err-bytes res) (count (slurp stderr))))
            (is (= 12 (count
                       (line-seq
                        (clojure.java.io/reader stderr)))))))))))
