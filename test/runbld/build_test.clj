(ns runbld.build-test
  (:require
   [clj-git.core :as git]
   [clojure.test :refer :all]
   [runbld.build :as build]
   [runbld.io :as io]
   [runbld.vcs.git :as vcs-git]
   [schema.test :as s]))

(use-fixtures :once schema.test/validate-schemas)

(deftest job-name-splitting
  (is
   (= {:branch "master"
       :job-name "elastic+elasticsearch+master+multijob-intake"
       :job-name-extra "multijob-intake"
       :org "elastic"
       :org-project-branch "elastic/elasticsearch#master"
       :project "elasticsearch"}
      (build/split-job-name
       "elastic+elasticsearch+master+multijob-intake")))
  (let [s (str "elastic+apm-agent-nodejs+master+multijob-tav-tests/"
               "NODEJS_VERSION=7,TAV=generic-pool+mysql+redis+koa-router"
               ",label=linux")]
    (is
     (= {:branch "master"
         :job-name s
         :job-name-extra (str "multijob-tav-tests/NODEJS_VERSION=7,TAV="
                              "generic-pool+mysql+redis+koa-router,label=linux")
         :org "elastic"
         :org-project-branch "elastic/apm-agent-nodejs#master"
         :project "apm-agent-nodejs"}
        (build/split-job-name s)))))

(deftest test-checkout
  (let [clone-dir "tmp/git/test-checkout"
        commit-time "2017-05-05T20:11:58Z"
        commit-id "9261b6332464bce8e63f6e42a61adb59062a56cc"
        commit-to-fetch {:vcs
                         {:commit-id commit-id
                          :commit-time "2017-05-05T20:11:58Z"}}
        vcs-repo (vcs-git/make-repo clone-dir "elastic" "runbld" "master")]
    (try
      (when (.exists (clojure.java.io/file clone-dir))
        (io/rmdir-r clone-dir))
      (git/git-clone clone-dir
                     "git@github.com:elastic/runbld.git"
                     ["--depth" "1"])
      (is (vcs-git/is-shallow? vcs-repo)
          "We should have a shallow repo.")
      (is (not (= commit-id (:commit-id (vcs-git/log-latest vcs-repo))))
          "The head commit should be different than some old commit.")
      (build/checkout-last-good-commit vcs-repo commit-to-fetch)
      (is (vcs-git/is-shallow? vcs-repo)
          "It's still shallow, just less so than before")
      (let [commit (vcs-git/log-latest vcs-repo)]
        (is (= (:commit-time commit) commit-time)
            "The commit time should be the one we specified")
        (is (= (:commit-id commit) commit-id)
            "The commit id should be the one we specified")
        (is (re-find #"Use a real template for text/plain content"
                     (:log-pretty commit))
            "The log should be what we expect, too."))
      (finally
        (when (.exists (clojure.java.io/file clone-dir))
          (io/rmdir-r clone-dir))))))
