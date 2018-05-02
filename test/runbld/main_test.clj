(ns runbld.main-test
  (:require
   [cheshire.core :as json]
   [clj-git.core :refer [git git-branch git-clone git-log
                         load-repo shallow-clone?]]
   [clj-http.core :as http-core]
   [clojure.java.io :as jio]
   [clojure.test :refer :all]
   [clojure.walk :refer [keywordize-keys]]
   [environ.core :as environ]
   [runbld.build :as build]
   [runbld.env :as env]
   [runbld.io :as io]
   [runbld.java :as java]
   [runbld.main :as main]
   [runbld.notifications.email :as email]
   [runbld.notifications.slack :as slack]
   [runbld.opts :as opts]
   [runbld.process :as proc]
   [runbld.scheduler :as scheduler]
   [runbld.scheduler.default :as default-sched]
   [runbld.scm :as scm]
   [runbld.store :as store]
   [runbld.test-support :as ts]
   [runbld.util.debug :as debug]
   [runbld.util.http :refer [wrap-retries]]
   [runbld.vcs.git :as git]
   [runbld.version :as version]
   [schema.test :as s]
   [stencil.core :as mustache]))

(def email (atom []))
(def slack (atom []))

(defn run [args]
  [(opts/parse-args args) (apply main/-main args)])

(defn slack-msg []
  (-> @slack
      json/decode
      keywordize-keys
      :attachments
      first))

(defn fake-slack-send [opts ctx]
  (reset! slack (slack/render opts ctx)))

(use-fixtures :once
  ts/dont-die-fixture)

(use-fixtures :each
  (ts/redirect-logging-fixture)
  ts/reset-debug-log-fixture)

(s/deftest main
  ;; Change root bindings for these Vars, affects any execution no
  ;; matter what thread
  (with-redefs [;; Don't really kill the JVM
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
  (with-redefs [main/really-die (fn [& args] :dontdie)
                proc/run (fn [& args] (throw
                                       (Exception.
                                        "boy that was unexpected")))]
    (git/with-tmp-repo [d "tmp/git/main-test-1"]
      (let [[opts res] (run ["-c" "test/config/main.yml"
                             "-j" "elastic+foo+master"
                             "-d" d
                             "/path/to/script.bash"])]
        (is (= String (type res)))
        (is (.startsWith res "#error {\n :cause boy that was "))))))

(s/deftest execution-with-defaults
  (testing "real execution all the way through"
    (with-redefs [email/send* (fn [& args]
                                (swap! email concat args)
                                ;; to satisfy schema
                                {})
                  slack/send fake-slack-send]
      (testing "build failure -- default notification settings"
        (git/with-tmp-repo [d "tmp/git/main-test-2"]
          (let [[opts res] (run
                             (conj
                              ["-c" "test/config/main.yml"
                               "-j" "elastic+foo+master"
                               "-d" d]
                              (if (opts/windows?)
                                "test/fail.bat"
                                "test/fail.bash")))]
            (is (= 1 (:exit-code res)))
            (is (= 1 (-> (store/get (-> opts :es :conn)
                                    (-> res :store-result :addr :index)
                                    (-> res :store-result :addr :type)
                                    (-> res :store-result :addr :id))
                         :process
                         :exit-code)))
            (let [[_ _ _ _ subj _ _] @email]
              (is (.startsWith subj "FAILURE"))
              (is (re-find (re-pattern (-> res :store-result :build-doc :id))
                           subj)))
            (is (.contains (:title (slack-msg)) "FAILURE")))))
      (testing "build success: default notification settings"
        (reset! email [])
        (reset! slack [])
        (git/with-tmp-repo [d "tmp/git/main-test-3"]
          (let [[opts res] (run
                             (conj
                              ["-c" "test/config/main.yml"
                               "-j" "elastic+foo+master"
                               "-d" d]
                              "test/success.bash"))]
            (is (= 0 (:exit-code res)))
            (is (= 0 (-> (store/get (-> opts :es :conn)
                                    (-> res :store-result :addr :index)
                                    (-> res :store-result :addr :type)
                                    (-> res :store-result :addr :id))
                         :process
                         :exit-code)))
            (is (empty? @email))
            (is (= (:color (slack-msg)) "good"))))))))

(s/deftest execution-with-slack-overrides
  (testing "slack overrides:"
    (with-redefs [email/send* (fn [& args]
                                (swap! email concat args)
                                ;; to satisfy schema
                                {})
                  slack/send fake-slack-send]
      (testing "build success: notify on first success after failure(s)"
        (reset! email [])
        (reset! slack [])
        ;; fail first
        (git/with-tmp-repo [d "tmp/git/main-test-4"]
          (let [[opts res] (run (conj
                                 ["-c" "test/config/slack.yml"
                                  "-j" "elastic+foo+master"
                                  "-d" d]
                                 "test/fail.bash"))]))
        ;; then succeed
        (reset! email [])
        (reset! slack [])
        (git/with-tmp-repo [d "tmp/git/main-test-5"]
          (let [[opts res] (run (conj
                                 ["-c" "test/config/slack.yml"
                                  "-j" "elastic+foo+master"
                                  "-d" d]
                                 "test/success.bash"))]
            (is (empty? @email))
            ;; we should get a slack notification
            (is (= (:color (slack-msg)) "good")))))
      (testing "build success: don't notify on subsequent successes"
        ;; succeed once more
        (reset! email [])
        (reset! slack [])
        (git/with-tmp-repo [d "tmp/git/main-test-6"]
          (let [[opts res] (run (conj
                                 ["-c" "test/config/slack.yml"
                                  "-j" "elastic+foo+master"
                                  "-d" d]
                                 "test/success.bash"))]
            ;; we shouldn't get any more notifications
            (is (empty? @email))
            (is (empty? @slack))))))))

(s/deftest last-good-commit
  (let [email-body (atom "no-email-body-yet")]
    (with-redefs [email/send* (fn [_ _ _ _ _ plain html attachments]
                                (reset! email-body html)
                                ;; to satisfy schema
                                {})
                  slack/send fake-slack-send]
      (testing "successful intake"
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
              (io/rmdir-r intake-dir)))))
      (reset! email-body "no-email-body-yet")
      (testing "the source is updated prior to checking out l-g-c"
        ;; The setup is when the last-good-commit is a commit "in the
        ;; future" from the point of view of the locally checked-out copy
        ;; of the repo.  In that case we get an exception with "fatal:
        ;; reference is not a tree" when we attempt to checkout the l-g-c
        ;; Simply fetching will resolve the issue.
        (let [origin "tmp/git/origin-repo"
              first-clone "tmp/git/first-clone"
              second-clone "tmp/git/second-clone"]
          (try
            (let [origin-repo (load-repo origin)
                  first-repo (load-repo first-clone)
                  second-repo (load-repo second-clone)]
              (.mkdirs (io/file (:path origin-repo)))
              (git origin-repo "init" ["--bare"])
              ;; Add a commit to the first repo- the one that will
              ;; soon be out of date
              (git-clone first-clone origin)
              (git/add-test-commit first-clone)
              (git first-repo "push")
              ;; now create a new clone that will have the previous
              ;; commit and push another new commit that first won't
              ;; have
              (git-clone second-clone origin)
              (git/add-test-commit second-clone)
              (git second-repo "push")
              ;; Run runbld on the second clone to store the l-g-c as
              ;; one that doesn't exist in the first
              (let [second-commit (:commit (git/head-commit second-repo))
                    [opts-second res-second]
                    (run (conj
                          ["-c" "test/config/main.yml"
                           "-j" "elastic+foo+master+intake"
                           "-d" second-clone]
                          (if (opts/windows?)
                            "test/success.bat"
                            "test/success.bash")))
                    _ (reset! email-body "no-email-body-yet")
                    ;; now run it on the first clone, which would
                    ;; throw an exception were it not for the added
                    ;; fetching
                    [opts-first res-first]
                    (run (conj
                          ["-c" "test/config/main.yml"
                           "-j" "elastic+foo+master+periodic"
                           "-d" first-clone
                           "--last-good-commit" "elastic+foo+master+intake"]
                          (if (opts/windows?)
                            "test/fail.bat"
                            "test/fail.bash")))]
                ;; make sure we saw what we expected to see
                (is (= 0 (:exit-code res-second)))
                (is (= 1 (:exit-code res-first)))
                (is (.contains @email-body
                               (str "using successful commit " second-commit))
                    (with-out-str
                      (println "@email-body")
                      (prn @email-body)))))
            (finally
              (io/rmdir-r origin)
              (io/rmdir-r first-clone)
              (io/rmdir-r second-clone))))))))

(s/deftest set-scm-build
  (testing "the scm build can be set multiple ways:"
    (let [branch (atom nil)]
      (with-redefs [git-clone (fn [local remote clone-args]
                                (reset! branch
                                        (get (apply hash-map clone-args)
                                             "--branch")))
                    scm/update-workspace (fn [local b depth]
                                           (reset! branch b))
                    scm/wipe-workspace (fn [workspace] nil)
                    scheduler/as-map identity]
        (testing "supplying branch in yml"
          (let [raw-opts (-> (opts/parse-args
                              ["-c" "test/config/scm.yml"
                               "-j" "owner+project+master"
                               "-d" "tmp/git/owner+project+branch"
                               "test/fail.bash"])
                             (assoc :logger runbld.io/log)
                             ;; make schema happy
                             (assoc :scheduler (default-sched/make {})))
                opts (build/add-build-info raw-opts)]
            (scm/bootstrap-workspace
             (assoc-in opts [:scm :wipe-workspace] false))
            (is (= "master"
                   (-> opts :scm :branch)
                   (-> opts :build :branch)
                   @branch))))
        (testing "extracting branch from job"
          (git/with-tmp-repo [d "tmp/git/owner+project+branch"]
            (let [raw-opts (-> (conj
                                ["-c" "test/config/scm.yml"
                                 ;; NOTE: the job name changed
                                 "-j" "owner+project+branch"
                                 "-d" "tmp/git/owner+project+branch"]
                                "test/fail.bash")
                               opts/parse-args
                               (assoc :logger runbld.io/log)
                               ;; make schema happy
                               (assoc :scheduler (default-sched/make {})))
                  opts (build/add-build-info raw-opts)]
              (scm/bootstrap-workspace
               (assoc-in opts [:scm :wipe-workspace] false))
              (is (nil? (-> opts :scm :branch)))
              (is (= "branch"
                     (-> opts :build :branch)
                     @branch)))))))))

(s/deftest ^:integration execution-with-scm
  (testing "real execution all the way through with cloning via scm config"
    (let [wipe-workspace-orig scm/wipe-workspace
          workspace "tmp/git/elastic+foo+master"
          master-commit (atom nil)]
      (with-redefs [email/send* (fn [& args]
                                  (swap! email concat args)
                                  ;; to satisfy schema
                                  {})
                    slack/send fake-slack-send
                    scm/wipe-workspace (fn [opts] opts)
                    scm/find-workspace (constantly workspace)]
        (try
          (let [[opts res] (run
                             (conj
                              ["-c" "test/config/scm.yml"
                               "-j" "elastic+foo+master"
                               "-d" workspace]
                              (if (opts/windows?)
                                "test/fail.bat"
                                "test/fail.bash")))]
            (testing "the elasticsearch repo was cloned correctly"
              (let [branch (-> opts :scm :branch)
                    depth (-> opts :scm :depth)
                    repo (load-repo workspace)]
                (is (= branch (git-branch repo)))
                (let [log (git-log repo)]
                  (is (= depth (count (git-log repo))))
                  (reset! master-commit (last log)))
                (is (shallow-clone? repo)))
              (testing "scm doesn't break anything else"
                (is (= 1 (:exit-code res)))
                (is (= 1 (-> (store/get (-> opts :es :conn)
                                        (-> res :store-result :addr :index)
                                        (-> res :store-result :addr :type)
                                        (-> res :store-result :addr :id))
                             :process
                             :exit-code)))
                (is (.startsWith
                     (let [[_ _ _ _ subj _ _] @email] subj) "FAILURE"))
                (is (.contains (:title (slack-msg)) "FAILURE")))
              (testing "Running again w/o wipe-workspace should update the repo"
                (let [[opts2 res2] (run
                                     (conj
                                      ["-c" "test/config/scm.yml"
                                       "-j" "elastic+foo+6.0"
                                       "-d" workspace]
                                      (if (opts/windows?)
                                        "test/fail.bat"
                                        "test/fail.bash")))
                      branch (-> opts2 :scm :branch)
                      depth (-> opts2 :scm :depth)
                      repo2 (load-repo workspace)]
                  (is (not (= (-> opts :scm :branch)
                              (-> opts2 :scm :branch))))
                  (is (= branch (git-branch repo2)))
                  (is (= depth (count (git-log repo2))))))
              (testing "can clone a specific commit"
                (with-redefs [environ/env
                              {:dev "true"
                               ;; this shouldn't have been fetched above
                               :branch-specifier (:parent @master-commit)}]
                  (let [[opts3 res3] (run
                                       (conj
                                        ["-c" "test/config/scm.yml"
                                         "-j" "elastic+foo+master"
                                         "-d" workspace]
                                        (if (opts/windows?)
                                          "test/fail.bat"
                                          "test/fail.bash")))
                        repo3 (load-repo workspace)]
                    (is (= (:parent @master-commit)
                           (:commit (first (git-log repo3)))))
                    (is (shallow-clone? repo3)))))
              (testing "branch is updated and indexed properly"
                (with-redefs [environ/env
                              {:dev "true"
                               ;; this shouldn't have been fetched above
                               :branch-specifier "6.0"}]
                  (let [[opts res] (run
                                     (conj
                                      ["-c" "test/config/scm.yml"
                                       "-j" "elastic+foo+master"
                                       "-d" workspace]
                                      (if (opts/windows?)
                                        "test/fail.bat"
                                        "test/fail.bash")))
                        repo (load-repo workspace)
                        slack-msg (get-in (json/parse-string @slack true)
                                          [:attachments 0 :title])]
                    (is (= "6.0" (git-branch repo)))
                    (is (= "6.0" (get-in res [:store-result :build-doc
                                              :build :branch])))
                    (is (re-find #"foo 6.0" slack-msg)))))
              (testing "wiping the workspace"
                (wipe-workspace-orig opts)
                ;; Should only have the top-level workspace dir left
                (is (= [workspace]
                       (map str (file-seq (jio/file workspace))))))))
          (finally
            (io/rmdir-r workspace)))))))

(deftest scm-basedir
  (let [wipe-workspace-orig scm/wipe-workspace
        user-dir (System/getProperty "user.dir")
        workspace "tmp/git/base+dir+master"
        master-commit (atom nil)]
    (try
      (with-redefs [main/really-die (fn [& args] :dontdie)
                    opts/config-file-defaults
                    (assoc-in opts/config-file-defaults
                              [:process :cwd]
                              (str user-dir "/" workspace))
                    email/send* (fn [& args]
                                  (swap! email concat args)
                                  ;; to satisfy schema
                                  {})
                    slack/send fake-slack-send
                    scm/wipe-workspace (fn [opts] opts)
                    scm/find-workspace (constantly workspace)]
        (let [[opts res] (run
                           (conj
                            ["-c" (str user-dir "/" "test/config/scm.yml")
                             "-j" "base+dir+master"]
                            (str user-dir "/" "test/check-cwd.bash")))]
          (testing "the elasticsearch repo was cloned correctly into the subdir"
            (let [branch (-> opts :scm :branch)
                  depth (-> opts :scm :depth)
                  repo (load-repo (get-in opts [:process :cwd]))]
              (is (= 0 (:exit-code res)))
              (is (re-find (re-pattern "/some/other/dir$")
                           (get-in opts [:process :cwd])))
              (is (= branch (git-branch repo)))
              (let [log (git-log repo)]
                (is (= depth (count (git-log repo))))
                (reset! master-commit (last log)))
              (is (shallow-clone? repo)))))
        (testing "setting the cwd from the command line take precedence"
          (let [[opts res] (run
                             (conj
                              ["-c" (str user-dir "/" "test/config/scm.yml")
                               "-j" "base+dir+master"
                               "-d" workspace]
                              (str user-dir "/" "test/success.bash")))]
            (is (= (io/abspath workspace)
                   (get-in opts [:process :cwd]))))))
      (finally
        (when (.exists (jio/file (str user-dir "/" workspace)))
          (io/rmdir-r (str user-dir "/" workspace)))))))

(deftest http-retries
  (let [call-count (atom 0)]
    (with-redefs [http-core/http-client (fn [& _]
                                          (swap! call-count inc)
                                          (throw (Exception. "oh noes!")))
                  email/send* (fn [& args]
                                (swap! email concat args)
                                ;; to satisfy schema
                                {})
                  slack/send fake-slack-send]
      (testing "The live path will retry correctly"
        (is (thrown-with-msg? Exception #"oh noes"
                              (git/with-tmp-repo [d "tmp/git/main-test-3"]
                                ;; this call to run will fail b/c of
                                ;; the redefined http-client call
                                ;; above
                                (run (conj ["-c" "test/config/main.yml"
                                            "-j" "elastic+foo+master"
                                            "-d" d]
                                           "test/success.bash")))))
        (is (= 20 @call-count)
            "Should've retried the default number of times"))
      (testing "Can configure the number of retries"
        (reset! call-count 0)
        (let [settings (merge
                        (:es opts/config-file-defaults)
                        {:url (str "http://" (java.util.UUID/randomUUID) ".io")
                         :additional-middleware [wrap-retries]
                         :retries-config {:sleep 50
                                          :tries 5}})
              conn (store/make-connection settings)]
          (is (thrown-with-msg? Exception #"oh noes"
                                (store/get conn "index" "type" 1234)))
          (is (= 5 @call-count))))
      (testing "Can configure the retried exceptions"
        (reset! call-count 0)
        (let [settings (merge
                        (:es opts/config-file-defaults)
                        {:url (str "http://" (java.util.UUID/randomUUID) ".io")
                         :additional-middleware [wrap-retries]
                         :retries-config {:sleep 50
                                          :tries 5}})
              conn (store/make-connection settings)]
          (is (thrown-with-msg? Exception #"oh noes"
                                (store/get conn "index" "type" 1234)))
          (is (= 5 @call-count)))))))

(deftest record-progress
  (let [ready (promise)
        proceed (promise)
        done (promise)
        res (atom nil)]
    (with-redefs [email/send-email identity
                  slack/send-slack (fn [opts]
                                     (deliver done true)
                                     opts)
                  scm/wipe-workspace (fn [opts]
                                       (reset! res opts)
                                       (deliver ready true)
                                       (is (deref proceed 6000 nil)
                                           "Timed out waiting to proceed")
                                       opts)]
      (git/with-tmp-repo [d "tmp/git/record-progress-test-1"]
        (let [args (conj
                    ["-c" "test/config/main.yml"
                     "-j" "elastic+foo+master"
                     "-d" d]
                    "test/success.bash")
              opts (opts/parse-args args)
              time-start (atom nil)]
          (future (apply main/-main args))
          (when (is (deref ready 4000 nil)
                    "Timed out waiting on pipeline")
            (testing "we've started, we should have a record"
              (let [record (store/get (-> opts :es :conn)
                                      (-> @res :store-result :addr :index)
                                      (-> @res :store-result :addr :type)
                                      (-> @res :store-result :addr :id))]
                (is (= "elastic" (-> record :build :org)))
                (is (-> record :process :time-start))
                (reset! time-start (-> record :process :time-start))
                (is (nil? (-> record :process :exit-code)))))
            ;; nudge the time so we can check that process :time-start changes
            (Thread/sleep 100)
            (deliver proceed true)
            (when (is (deref done 10000 nil)
                      "Timed out waiting on pipeline to finish")
              (let [record (store/get (-> opts :es :conn)
                                      (-> @res :store-result :addr :index)
                                      (-> @res :store-result :addr :type)
                                      (-> @res :store-result :addr :id))]
                (is (-> record :process :time-start))
                (is (not (= (-> record :process :time-start)
                            @time-start)))
                (is (= "elastic" (-> record :build :org)))
                (is (zero? (-> record :process :exit-code)))))))))))

(deftest debug-log
  (with-redefs [main/really-die (fn [& args] :dontdie)
                email/send* (fn [& args]
                              ;; to satisfy schema
                              {})
                slack/send (fn [opts ctx])]
    (testing "debug log is capturing info, rethrowing correctly"
      (with-redefs [runbld.opts/load-config
                    (fn [_]
                      (slingshot.slingshot/throw+
                       {:error :runbld.opts/file-not-found
                        :msg "on purpose test failure"}))]
        (git/with-tmp-repo [d "tmp/git/debug-log-1"]
          (try
            (let [res (apply main/-main
                             ["-c" "test/config/main.yml"
                              "-j" "elastic+foo+master"
                              "-d" d
                              "test/success.bash"])]
              (is (= "on purpose test failure" res))
              (is (= 1 (count (debug/get-log))))
              (is (.contains (first (debug/get-log))
                             "runbld.main"))
              (is (.contains (first (debug/get-log))
                             (version/version))))
            (catch Throwable t
              ;; with-debug-log rethrows but it should rethrow in a way
              ;; that runbld.main can continue to work as it should
              (is false "This should've been caught in runbld.main"))))))
    ;; This test is disabled b/c it requires a real email config but
    ;; is left here for future manual testing
    #_
    (testing "emails are sent"
      (debug/reset)
      (with-redefs [store/store-result
                    (fn [& _]
                      (throw (Exception. "on purpose test failure")))]
        (git/with-tmp-repo [d "tmp/git/debug-log-2"]
          (try
            (let [res (apply main/-main
                             ["-c" "test/config/debug.yml"
                              "-j" "elastic+foo+master"
                              "-d" d
                              "test/success.bash"])]
              (is (and res (.contains res "on purpose test failure")))
              (is (< 1 (count (debug/get-log))))
              (when (is (first (debug/get-log)))
                (is (.contains (first (debug/get-log))
                               "runbld.main"))
                (is (.contains (first (debug/get-log))
                               (version/version)))))
            (catch Throwable t
              ;; with-debug-log rethrows but it should rethrow in a way
              ;; that runbld.main can continue to work as it should
              (is false "This should've been caught in runbld.main"))))))))

(deftest build-metadata
  (let [email-body (atom "no-email-body-yet")
        intake-dir "tmp/git/metadata-intake"
        periodic-dir "tmp/git/metadata-periodic"]
    (try
      (with-redefs [email/send* (fn [_ _ _ _ _ plain html attachments]
                                  (reset! email-body html)
                                  ;; to satisfy schema
                                  {})
                    slack/send fake-slack-send]
        (git/init-test-clone periodic-dir intake-dir)
        (let [[opts-intake res-intake]
              (run (conj
                    ["-c" "test/config/main.yml"
                     "-j" "elastic+foo+master+intake"
                     "-d" intake-dir]
                    "test/add-metadata.bash"))
              [opts-periodic res-periodic]
              (run (conj
                    ["-c" "test/config/main.yml"
                     "-j" "elastic+foo+master+periodic"
                     "-d" periodic-dir
                     "--last-good-commit"
                     "elastic+foo+master+intake"]
                    "test/check-metadata.bash"))]
          (is (= 0 (:exit-code res-intake)))
          (let [stored-metadata (get-in
                                 (store/get
                                  (-> opts-intake :es :conn)
                                  (-> res-intake :store-result :addr :index)
                                  (-> res-intake :store-result :addr :type)
                                  (-> res-intake :store-result :addr :id))
                                 [:build :metadata])]
            (is (= #{"the first metadata" "the second metadata"}
                   (set (clojure.string/split stored-metadata #";")))
                "the metadata should be stored."))
          ;; 0 exit code means the second script got the
          ;; metadata- the check is in the bash script
          (is (= 0 (:exit-code res-periodic))
              (str "the metadata should be in the environment\n"
                   "the test is in check-metadata.bash"))
          (testing "existing BUILD_METADATA environment is respected"
            (with-redefs [environ/env (assoc environ/env
                                             :build-metadata
                                             "existing;metadata")]
              (let [[opts-periodic res-periodic]
                    (run (conj
                          ["-c" "test/config/main.yml"
                           "-j" "elastic+foo+master+periodic"
                           "-d" periodic-dir
                           "--last-good-commit" "elastic+foo+master+intake"]
                          "test/check-existing-metadata.bash"))]
                ;; 0 exit code means the second script got the
                ;; metadata- the check is in the bash script
                (is (= 0 (:exit-code res-periodic))
                    (str "the metadata was incorrect in the environment\n"
                         "the test is in check-existing-metadata.bash")))))))
      (finally
        (io/rmdir-r periodic-dir)
        (io/rmdir-r intake-dir)))))

(deftest disabled-build-metadata
  (let [set-calls (atom 0)
        record-calls (atom 0)]
    (with-redefs [build/set-build-meta-environment (fn [opts]
                                                     (swap! set-calls inc)
                                                     opts)
                  build/record-build-meta (fn [opts]
                                            (swap! record-calls inc)
                                            opts)]
      (testing "metadata fns are skipped when disabled"
        (git/with-tmp-repo [d "tmp/git/disabled-metadata-test"]
          (let [[opts res] (run
                             (conj
                              ["-c" "test/config/main.yml"
                               "-j" "disabled-metadata+foo+master"
                               "-d" d]
                              "test/success.bash"))]
            (is (= 0 (:exit-code res)))
            (is (= 0 @set-calls))
            (is (= 0 @record-calls)))))
      (testing "metadata fns are called as expected when enabled"
        (reset! set-calls 0)
        (reset! record-calls 0)
        (git/with-tmp-repo [d "tmp/git/disabled-metadata-test"]
          (let [[opts res] (run
                             (conj
                              ["-c" "test/config/main.yml"
                               "-j" "enabled-metadata+foo+master"
                               "-d" d]
                              "test/success.bash"))]
            (is (= 0 (:exit-code res)))
            (is (= 1 @set-calls))
            (is (= 1 @record-calls))))))))

(s/deftest user-specified-commit
  ;; This tests that branch_specifier is honored.  we basically need a
  ;; repo with several commits where lgc is some commit A and
  ;; branch_specifier points to commit B
  (let [script (if (opts/windows?) "test/success.bat" "test/success.bash")
        job-name "elastic+foo+master+user-specified"
        source-dir "/tmp/runbld/user-source"
        dest-dir "/tmp/runbld/user-dest"]
    ;; because we need to use the scm feature we need the repo to be
    ;; somewhere on disk at a known location, therefore we should make
    ;; sure to clean up before trying anything
    (if (.exists (jio/file dest-dir))
      (io/rmdir-r dest-dir))
    (if (.exists (jio/file source-dir))
      (io/rmdir-r source-dir))
    (try
      ;; create the dummy repo in source-dir and clone it to dest-dir
      (git/init-test-clone dest-dir source-dir)
      (let [first-commit (:commit-id (git/head-commit source-dir))
            [opts-1 res-1]
            (run ["-c" "test/config/main.yml"
                  "-j" job-name
                  "-d" dest-dir
                  script])
            ;; at this point we should have a l-g-c at the first
            ;; commit, so we will add a second commit that the
            ;; user can specify with branch_specifier
            lgc (get-in (build/last-good-build
                         job-name
                         opts-1
                         (runbld.vcs.middleware/make-repo
                          {:process {:cwd source-dir}
                           :build {:org "elastic"
                                   :project "foo"
                                   :branch "master"}}))
                        [:vcs :commit-id])
            _ (git/add-test-commit source-dir)
            second-commit (:commit-id (git/head-commit source-dir))
            ;; we will also add a third commit to ensure we aren't
            ;; accidentally getting the head commit later
            _ (git/add-test-commit source-dir)
            third-commit (:commit-id (git/head-commit source-dir))]
        (is (= 0 (:exit-code res-1)))
        (is (= lgc first-commit))
        ;; specify the second commit in the "environment" and run the
        ;; script again, specifying that we want the l-g-c
        (with-redefs [environ/env {:dev "true"
                                   :branch-specifier second-commit}]
          (let [[opts-2 res-2]
                (run ["-c" "test/config/main.yml"
                      "-j" job-name
                      "-d" dest-dir
                      "--last-good-commit" job-name
                      script])]
            (is (= 0 (:exit-code res-2)))
            (is (= second-commit
                   (get-in res-2 [:store-result
                                  :build-doc
                                  :vcs
                                  :commit-id])))))
        ;; user specified commits should not affect the
        ;; last-good-commit
        (is (= lgc
               (get-in (build/last-good-build
                        job-name
                        opts-1
                        (runbld.vcs.middleware/make-repo
                         {:process {:cwd source-dir}
                          :build {:org "elastic"
                                  :project "foo"
                                  :branch "master"}}))
                       [:vcs :commit-id]))))
      (finally
        (io/rmdir-r dest-dir)
        (io/rmdir-r source-dir)))))

(s/deftest java-home-in-path
  (let [logs (atom [])
        orig-save-logs store/save-logs!]
    (with-redefs [store/save-logs! (fn [opts docs]
                                     (reset! logs docs)
                                     (orig-save-logs opts docs))]
      (git/with-tmp-repo [d "tmp/git/java-home-test"]
        (let [java-home (:home (java/jvm-facts))
              [opts res] (run
                           (conj
                            ["-c" "test/config/main.yml"
                             "-j" "elastic+foo+master"
                             "-d" d]
                            "test/echo-path.sh"))]
          (is (= 0 (:exit-code res)))
          (is  (->> @logs
                    (map :log)
                    (filter #(re-find #"^PATH: " %))
                    first
                    (re-find (re-pattern java-home)))))))))
