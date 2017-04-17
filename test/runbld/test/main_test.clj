(ns runbld.test.main-test
  (:require [schema.test :as s])
  (:require [clojure.test :refer :all]
            [clojure.walk :refer [keywordize-keys]]
            [runbld.build :as build]
            [runbld.notifications.email :as email]
            [runbld.notifications.slack :as slack]
            [runbld.env :as env]
            [runbld.opts :as opts]
            [runbld.process :as proc]
            [runbld.store :as store]
            [runbld.io :as io]
            [runbld.vcs.git :as git]
            [runbld.version :as version]
            [stencil.core :as mustache]
            [cheshire.core :as json])
  (:require [runbld.main :as main] :reload-all))

(def email (atom []))
(def slack (atom []))

(defn run [args]
  [(opts/parse-args args) (apply main/-main args)])

(defn slack-msg []
  (-> @slack
      json/decode
      keywordize-keys
      :attachments
      first
      :title))

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

(s/deftest execution-with-defaults
  (testing "real execution all the way through"
    (with-redefs [io/log (fn [& x] (prn x))
                  email/send* (fn [& args]
                                (swap! email concat args)
                                ;; to satisfy schema
                                {})
                  slack/send (fn [opts ctx]
                               (let [f (-> opts :slack :template)
                                     tmpl (-> f io/resolve-resource slurp)
                                     color (if (-> ctx :process :failed)
                                             "danger"
                                             "good")
                                     js (mustache/render-string
                                         tmpl (assoc ctx :color color))]
                                 (reset! slack js)))]
      (testing "build failure -- default notification settings"
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
                                    (-> res :store-result :addr :index)
                                    (-> res :store-result :addr :type)
                                    (-> res :store-result :addr :id))
                         :process
                         :exit-code)))
            (is (.startsWith
                 (let [[_ _ _ subj _ _] @email] subj) "FAILURE"))
            (is (.contains (slack-msg) "FAILURE")))))
      (testing "build success: default notification settings"
        (reset! email [])
        (reset! slack [])
        (git/with-tmp-repo [d "tmp/git/main-test-3"]
          (let [args (conj
                      ["-c" "test/config/main.yml"
                       "-j" "elastic+foo+master"
                       "-d" d]
                      "test/success.bash")
                opts (opts/parse-args args)
                res (apply main/-main args)]
            (is (= 0 (:exit-code res)))
            (is (= 0 (-> (store/get (-> opts :es :conn)
                                    (-> res :store-result :addr :index)
                                    (-> res :store-result :addr :type)
                                    (-> res :store-result :addr :id))
                         :process
                         :exit-code)))
            (is (empty? @email))
            (is (.contains (slack-msg) "SUCCESS"))))))))

(s/deftest execution-with-slack-overrides
  (testing "slack overrides:"
    (with-redefs [io/log (fn [& _] :noconsole)
                  email/send* (fn [& args]
                                (swap! email concat args)
                                ;; to satisfy schema
                                {})
                  slack/send (fn [opts ctx]
                               (let [f (-> opts :slack :template)
                                     tmpl (-> f io/resolve-resource slurp)
                                     color (if (-> ctx :process :failed)
                                             "danger"
                                             "good")
                                     js (mustache/render-string
                                         tmpl (assoc ctx :color color))]
                                 (reset! slack js)))]
      (testing "build success: notify on first success after failure(s)"
        (reset! email [])
        (reset! slack [])
        ;; fail first
        (git/with-tmp-repo [d "tmp/git/main-test-4"]
          (let [args (conj
                      ["-c" "test/config/slack.yml"
                       "-j" "elastic+foo+master"
                       "-d" d]
                      "test/fail.bash")
                opts (opts/parse-args args)
                res (apply main/-main args)]))
        ;; then succeed
        (reset! email [])
        (reset! slack [])
        (git/with-tmp-repo [d "tmp/git/main-test-5"]
          (let [args (conj
                      ["-c" "test/config/slack.yml"
                       "-j" "elastic+foo+master"
                       "-d" d]
                      "test/success.bash")
                opts (merge (opts/parse-args args)
                            {:slack
                             {:success false}})
                res (apply main/-main args)]
            (is (empty? @email))
            ;; we should get a slack notification
            (is (.contains (slack-msg) "SUCCESS")))))
      (testing "build success: don't notify on subsequent successes"
        ;; succeed once more
        (reset! email [])
        (reset! slack [])
        (git/with-tmp-repo [d "tmp/git/main-test-6"]
          (let [args (conj
                      ["-c" "test/config/slack.yml"
                       "-j" "elastic+foo+master"
                       "-d" d]
                      "test/success.bash")
                res (apply main/-main args)]
            ;; we shouldn't get any more notifications
            (is (empty? @email))
            (is (empty? @slack))))))))

(s/deftest last-good-commit
  (testing "last-good-commit:"
    (testing "successful intake"
      (let [email-body (atom :no-email-body-yet)]
        (with-redefs [io/log (fn [& x] (prn x))
                      email/send* (fn [_ _ _ _ _ html _]
                                    (reset! email-body html)
                                    ;; to satisfy schema
                                    {})
                      slack/send (fn [& _] (prn 'slack))]
          (let [intake-dir "tmp/git/main-intake"
                periodic-dir "tmp/git/main-periodic"]
            (try
              (git/init-test-clone periodic-dir intake-dir)
              (let [[opts-intake res-intake]
                    (run (conj
                          ["-c" "test/config/main.yml"
                           "-j" "elastic+foo+master+intake"
                           "-d" intake-dir]
                          (if (opts/windows?)
                            "test/success.bat"
                            "test/success.bash")))]
                (let [[opts-periodic res-periodic]
                      (run (conj
                            ["-c" "test/config/main.yml"
                             "-j" "elastic+foo+master+periodic"
                             "-d" periodic-dir
                             "--last-good-commit" "elastic+foo+master+intake"]
                            (if (opts/windows?)
                              "test/fail.bat"
                              "test/fail.bash")))]
                  (is (= 0 (:exit-code res-intake)))
                  (is (= 1 (:exit-code res-periodic)))
                  (is (.contains @email-body "using successful commit")
                      (with-out-str
                        (println "@email-body")
                        (prn @email-body)))))
              (finally
                (io/rmdir-r periodic-dir)
                (io/rmdir-r intake-dir)))))))))


