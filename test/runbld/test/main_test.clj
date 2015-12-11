(ns runbld.test.main-test
  (:require [schema.test])
  (:require [clojure.test :refer :all]
            [runbld.build :as build]
            [runbld.env :as env]
            [runbld.opts :as opts]
            [runbld.process :as proc]
            [runbld.vcs.git :as git]
            [runbld.version :as version])
  (:require [runbld.main :as main] :reload-all))

(use-fixtures :once schema.test/validate-schemas)

(deftest main
  ;; Change root bindings for these Vars, affects any execution no
  ;; matter what thread
  (with-redefs [;; Don't pollute the console
                main/log (fn [_] :noconsole)
                ;; Don't really kill the JVM
                main/really-die (fn [& args] :dontdie)
                ;; Don't really execute an external process
                proc/run (fn [& args] :bogus)]
    (testing "version"
      (is (.startsWith (main/-main "-v") (version/version))))

    (testing "usage"
      (is (.startsWith (main/-main) "runbld ")))

    (testing "bad config file"
      (is (.startsWith (main/-main "-c" "/tmp/noexist"
                                   "-j" "elastic+foo+master"
                                   "/path/to/script.bash") "config file ")))))

(deftest unexpected
  (with-redefs [main/log (fn [_] :noconsole)
                main/really-die (fn [& args] :dontdie)
                proc/run (fn [& args] (throw
                                       (Exception.
                                        "boy that was unexpected")))]
    (git/with-tmp-repo [d "tmp/git/main-test-1"]
      (let [args ["-c" "test/config/main.yml"
                  "-j" "elastic+foo+master"
                  "-d" d
                  "/path/to/script.bash"]
            opts (opts/parse-args args)
            res (apply main/-main args)]
        (is (= String (type res)))
        (is (.startsWith res "#error {\n :cause boy that was "))))))

(deftest execution
  (testing "real execution all the way through"
    (with-redefs [main/log (fn [_] :noconsole)]
      (git/with-tmp-repo [d "tmp/git/main-test-2"]
        (let [args ["-c" "test/config/main.yml"
                    "-j" "elastic+foo+master"
                    "-d" d
                    "test/success.bash"]
              opts (opts/parse-args args)]
          (is (= 0
                 (:exit-code
                  (binding [*out* (java.io.StringWriter.)
                            *err* (java.io.StringWriter.)]
                    (apply main/-main args))))))))))
