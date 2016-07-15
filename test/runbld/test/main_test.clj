(ns runbld.test.main-test
  (:require [schema.test :as s])
  (:require [clojure.test :refer :all]
            [runbld.build :as build]
            [runbld.notifications.email :as email]
            [runbld.env :as env]
            [runbld.opts :as opts]
            [runbld.process :as proc]
            [runbld.store :as store]
            [runbld.io :as io]
            [runbld.vcs.git :as git]
            [runbld.version :as version])
  (:require [runbld.main :as main] :reload-all))

(s/deftest main
  ;; Change root bindings for these Vars, affects any execution no
  ;; matter what thread
  (with-redefs [;; Don't pollute the console
                io/log (fn [& _] :noconsole)
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

(s/deftest unexpected
  (with-redefs [io/log (fn [& _] :noconsole)
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

(s/deftest execution
  (testing "real execution all the way through"
    (let [email (atom [])]
      (with-redefs [io/log (fn [& _] :noconsole)
                    email/send* (fn [& args]
                                  (swap! email concat args)
                                  ;; to satisfy schema
                                  {})]
        (git/with-tmp-repo [d "tmp/git/main-test-2"]
          (let [args (conj
                      ["-c" "test/config/main.yml"
                       "-j" "elastic+foo+master"
                       "-d" d]
                      (if (opts/windows?)
                        "test/fail.bat"
                        "test/fail.bash"))
                opts (opts/parse-args args)
                res (apply main/-main args)]
            (is (= 1 (:exit-code res)))
            (is (= 1 (-> (store/get (-> opts :es :conn)
                                    (-> res :store-result :addr))
                         :process
                         :exit-code)))
            (is (.startsWith
                 (let [[_ _ _ subj _ _] @email] subj) "FAILURE"))))))))
