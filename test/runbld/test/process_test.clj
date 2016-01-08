(ns runbld.test.process-test
  (:require [clojure.test :refer :all]
            [schema.test])
  (:require [runbld.java :as java]
            [runbld.opts :as opts]
            [runbld.process :as proc]
            [runbld.store :as store]
            [runbld.util.data :as data]
            [runbld.util.io :as io] :reload-all))

(use-fixtures :once schema.test/validate-schemas)

(deftest output-io
  (io/with-tmp-dir [dir ["tmp" (str *ns* "-")]]
    (let [master (io/file dir "master.log")
          stdout (io/file dir ".stdout.log")
          stderr (io/file dir ".stderr.log")
          args ["-c" "test/config/main.yml"
                "-j" "elastic+foo+master"
                "-d" (str dir)
                "-a" "-e"
                "--java-home" (java/java-home)
                "test/output.bash"]
          opts (opts/parse-args args)
          build-id (str (java.util.UUID/randomUUID))]
      (with-open [out (java.io.PrintWriter. master)]
        (let [res (binding [*out* out
                            *err* (java.io.StringWriter.)]
                    (proc/exec-with-capture
                     build-id
                     (-> opts :process :program)
                     (-> opts :process :args)
                     (-> opts :process :scriptfile)
                     (-> opts :process :cwd)
                     (-> opts :process :stdout)
                     (-> opts :process :stderr)
                     (-> opts :es)
                     {"JAVA_HOME" (opts :java-home)}))]
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
            (is (= 10 (count
                       (line-seq
                        (clojure.java.io/reader master))))))
          (testing "stdout"
            (is (= (:out-bytes res) (count (slurp stdout)))
                (str "the number bytes counted from the stream "
                     "differs from the number in "
                     stdout))
            (is (= 10 (count
                       (line-seq
                        (clojure.java.io/reader stdout)))))
            (is (= 10 (store/count-logs opts "stdout" build-id))))
          (testing "stderr"
            (is (= (:err-bytes res) (count (slurp stderr))))
            (is (= 1 (count
                       (line-seq
                        (clojure.java.io/reader stderr)))))))))))
