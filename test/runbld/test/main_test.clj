(ns runbld.test.main-test
  (:require [clojure.test :refer :all]
            [runbld.env :as env]
            [runbld.opts :as opts]
            [runbld.process :as proc]
            [runbld.publish :as publish]
            [runbld.vcs.git :as git]
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
                proc/run (fn [& args] :bogus)
                ;; facter is too slow
                env/facter (fn [& args] {:some :fact})
                ;; Don't really publish things
                publish/publish* (fn [& args] {:published :not-really})]
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
        (let [args ["-c" "test/runbld.yaml"
                    "--job-name" "elastic,proj1,master"
                    "/path/to/script.bash"]
              opts (opts/parse-args args)
              repo (git/init-test-repo (get-in opts [:profiles
                                                     :elastic-proj1-master
                                                     :git :remote]))
              res (apply main/-main args)]
          (is (.startsWith res "#error {\n :cause boy that was ")))))))

(deftest execution
  (testing "real execution all the way through"
    (with-redefs [main/log (fn [_] :noconsole)
                  env/facter (fn [& args] {:some :fact})
                  publish/publish* (fn [& args] {:published :not-really})]
      (is (= 0 (-> (main/-main "--job-name" "foo,bar,baz"
                               "test/success.bash") :process :status))))))
