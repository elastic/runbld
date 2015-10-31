(ns runbld.test.main-test
  (:require [clojure.test :refer :all]
            [runbld.process :as proc]
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
                proc/run (fn [script] {:cmd [script]
                                       :duration-millis 1
                                       :status 0})]
    (testing "normal execution"
      (is (= 0 (:status (main/-main "/path/to/script.bash")))))

    (testing "version"
      (is (.startsWith (main/-main "-v") (version/version))))

    (testing "usage"
      (is (.startsWith (main/-main) "runbld ")))

    (testing "unexpected exception"
      (with-redefs [proc/run (fn [& args] (throw
                                           (Exception.
                                            "boy that was unexpected")))]
        (is (.startsWith (main/-main "/path/to/script.bash")
                         "#error {\n :cause boy that was "))))))
