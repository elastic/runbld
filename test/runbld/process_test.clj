(ns runbld.process-test
  (:require
   [cheshire.core :as json]
   [clojure.test :refer :all]
   [runbld.io :as io]
   [runbld.java :as java]
   [runbld.opts :as opts]
   [runbld.process :as proc]
   [runbld.store :as store]
   [runbld.test-support :as ts]
   [runbld.util.data :as data]
   [schema.test]))

(use-fixtures :once schema.test/validate-schemas)
(use-fixtures :each (ts/redirect-logging-fixture))

(deftest output-io
  (io/with-tmp-dir [dir ["tmp" (str *ns* "-")]]
    (let [args (concat ["-j" "elastic+foo+master"
                        "-d" (str dir)]
                       (if (opts/windows?)
                         ["-c" "test\\config\\main.yml"
                          "test\\output.bat"]
                         ["-c" "test/config/main.yml"
                          "-a" "-e"
                          "test/output.bash"]))
          opts (opts/parse-args args)
          build-id (str (java.util.UUID/randomUUID))
          master (io/file dir "master.log")
          output (io/file dir (-> opts :process :output))
          err (java.io.StringWriter.)]
      (with-open [out (java.io.PrintWriter. master)]
        (let [res (binding [proc/*process-out* out
                            proc/*process-err* err]
                    (proc/run
                      (-> opts
                          (assoc-in [:id] build-id)
                          (assoc-in [:es :bulk-timeout-ms] 50)
                          (assoc-in [:es :bulk-size] 5))))]
          #_(println (slurp output))
          #_(println "MASTER:" (slurp master))
          #_(println "ERROR:" (str err))
          (testing "test should produce output"
            (is (= 11
                   (count
                    (line-seq (io/reader master))))
                (slurp master)))
          (testing "check output.log"
            (let [lines (->> output io/reader line-seq (map json/decode))]
              (is (= 12 (count lines)))
              (is (= (range 1 13)
                     (->> lines
                          (map #(get-in % ["ord" "total"]))
                          sort)))))
          (is (= 11 (store/count-logs opts "stdout" build-id)))
          (testing "env threading"
            (is (.contains (slurp master) "RUNBLD_TEST"))))))))

(deftest process-timeout
  (binding [*out* (java.io.StringWriter.)
            *err* (java.io.StringWriter.)]
    (io/with-tmp-dir [dir ["tmp" (str *ns* "-")]]
      (testing "timeouts are handled"
        (let [opts (opts/parse-args
                    (concat ["-j" "elastic+foo+master"
                             "-d" (str dir)]
                            (if (opts/windows?)
                              ["-c" "test\\config\\timeout.yml"
                               "test\\timeout.bat"]
                              ["-c" "test/config/timeout.yml"
                               "test/timeout.bash"])))
              {{:keys [took exit-code status]} :process-result}
              (proc/run (assoc opts :id (str (java.util.UUID/randomUUID))))]
          (is (> 5000 took)
              "The proc should be stopped near the timeout or 2 sec")
          (is (= "TIMEOUT" status))
          (is (= 1 exit-code))))
      (testing "processes work as normal when they don't timeout"
        (let [opts (opts/parse-args
                    (concat ["-j" "elastic+foo+master"
                             "-d" (str dir)]
                            (if (opts/windows?)
                              ["-c" "test\\config\\timeout.yml"
                               "test\\success.bat"]
                              ["-c" "test/config/timeout.yml"
                               "test/success.bash"])))
              {{:keys [exit-code status]} :process-result}
              (proc/run (assoc opts :id (str (java.util.UUID/randomUUID))))]
          (is (= "SUCCESS" status))
          (is (= 0 exit-code)))
        (let [opts (opts/parse-args
                    (concat ["-j" "elastic+foo+master"
                             "-d" (str dir)]
                            (if (opts/windows?)
                              ["-c" "test\\config\\timeout.yml"
                               "test\\fail.bat"]
                              ["-c" "test/config/timeout.yml"
                               "test/fail.bash"])))
              {{:keys [exit-code status]} :process-result}
              (proc/run (assoc opts :id (str (java.util.UUID/randomUUID))))]
          (is (= "FAILURE" status))
          (is (= 1 exit-code)))))))
