(ns runbld.test.process-test
  (:require [clojure.test :refer :all]
            [schema.test])
  (:require [cheshire.core :as json]
            [runbld.java :as java]
            [runbld.opts :as opts]
            [runbld.process :as proc]
            [runbld.store :as store]
            [runbld.util.data :as data]
            [runbld.io :as io] :reload-all))

(use-fixtures :once schema.test/validate-schemas)

(deftest output-io
  (io/with-tmp-dir [dir ["tmp" (str *ns* "-")]]
    (let [args ["-c" "test/config/main.yml"
                "-j" "elastic+foo+master"
                "-d" (str dir)
                "-a" "-e"
                "--java-home" (java/java-home)
                "test/output.bash"]
          opts (opts/parse-args args)
          build-id (str (java.util.UUID/randomUUID))
          master (io/file dir "master.log")
          output (io/file dir (-> opts :process :output))]
      (with-open [out (java.io.PrintWriter. master)]
        (let [res (binding [*out* out
                            *err* (java.io.StringWriter.)]
                    (proc/exec-with-capture
                     (-> opts :process :program)
                     (-> opts :process :args)
                     (-> opts :process :scriptfile)
                     (-> opts :process :cwd)
                     (-> opts :process :output)
                     (-> (-> opts :es)
                         (assoc :bulk-timeout-ms 50)
                         (assoc :bulk-size 5))
                     {"JAVA_HOME" (opts :java-home)}
                     {:build-id build-id}))]
          #_(println (slurp output))
          #_(println (slurp master))
          (testing "test should produce output"
            (is (= 10
                   (count
                    (line-seq (io/reader master))))))
          (testing "check output.log"
            (let [lines (->> output io/reader line-seq (map json/decode))]
              (is (= 11 (count lines)))
              (is (= (range 1 12)
                     (->> lines
                          (map #(get-in % ["ord" "total"]))
                          sort)))))
          (is (= 10 (store/count-logs opts "stdout" build-id))))))))
