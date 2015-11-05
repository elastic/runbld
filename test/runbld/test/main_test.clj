(ns runbld.test.main-test
  (:require [clojure.test :refer :all]
            [runbld.env :as env]
            [runbld.process :as proc]
            [runbld.publish :as publish]
            [runbld.version :as version])
  (:require [runbld.main :as main] :reload-all))

(deftest main
  ;; Change root bindings for these Vars, affects any execution no
  ;; matter what thread
  (with-redefs [;; Don't pollute the console
                main/log (fn [_] :noconsole)
                ;; Don't really kill the JVM
                main/really-die (fn [& args] :dontdie)
                ;; Don't really execute an external process
                proc/run (fn [script]
                           {:cmd [script]
                            :duration-millis 1
                            :status 0})
                ;; facter is too slow
                env/facter (fn [& args] {:some :fact})
                ;; Don't really publish things
                publish/publish* (fn [& args] {:published :not-really})]
    (testing "normal execution"
      (is (= 0 (-> (main/-main "-c" "test/runbld.yaml"
                               "/path/to/script.bash") :proc :status))))

    (testing "version"
      (is (.startsWith (main/-main "-v") (version/version))))

    (testing "usage"
      (is (.startsWith (main/-main) "runbld ")))

    (testing "bad config file"
      (is (.startsWith (main/-main "-c" "/tmp/noexist"
                                   "/path/to/script.bash") "config file ")))

    (testing "unexpected exception"
      (with-redefs [proc/run (fn [& args] (throw
                                           (Exception.
                                            "boy that was unexpected")))]
        (is (.startsWith (main/-main "-c" "test/runbld.yaml"
                                     "/path/to/script.bash")
                         "#error {\n :cause boy that was "))))))

(deftest execution
  (testing "real execution all the way through"
    (with-redefs [main/log (fn [_] :noconsole)
                  env/facter (fn [& args] {:some :fact})
                  publish/publish* (fn [& args] {:published :not-really})]
      (is (= 0 (-> (main/-main "--default-job-name" "foo,bar,baz"
                               "test/success.bash") :proc :status))))))
