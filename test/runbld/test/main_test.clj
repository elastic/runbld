(ns runbld.test.main-test
  (:require [clojure.test :refer :all]
            [runbld.process :as proc]
            [runbld.publish.elasticsearch :as elasticsearch]
            [runbld.publish.email :as email]
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
                ;; Don't really publish things
                elasticsearch/index (fn [_] {:in :fake-publish})
                email/send (fn [_] {:sent :email})]
    (testing "normal execution"
      (is (= 0 (-> (main/-main "/path/to/script.bash") :proc :status))))

    (testing "version"
      (is (.startsWith (main/-main "-v") (version/version))))

    (testing "usage"
      (is (.startsWith (main/-main) "runbld ")))

    (testing "config file"
      (is (= 0 (-> (main/-main "-c" "test/runbld.yaml" "/path/to/script.bash")
                   :proc
                   :status)))
      (is (.startsWith (main/-main "-c" "/tmp/noexist"
                                   "/path/to/script.bash") "config file ")))

    (testing "unexpected exception"
      (with-redefs [proc/run (fn [& args] (throw
                                           (Exception.
                                            "boy that was unexpected")))]
        (is (.startsWith (main/-main "/path/to/script.bash")
                         "#error {\n :cause boy that was "))))))
