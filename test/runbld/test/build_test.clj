(ns runbld.test.build-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as jio]
            [clj-jgit.porcelain :as jgit])
  (:require [runbld.build :as build]
            [runbld.opts :as opts]
            [runbld.util.io :as io]
            [runbld.vcs.git :as git]
            :reload-all))

(deftest build-info
  (let [opts ((build/wrap-build-meta
               (build/wrap-merge-profile
                identity))
              (opts/parse-args ["-c" "test/runbld.yaml"
                                "--job-name" "elastic,proj1,master"
                                "/path/to/script.bash"]))]
    (is (= "test/templates/proj1.mustache"
           (-> opts :email :template)))
    (is (= "foo@example.com"
           (-> opts :email :to)))))

(deftest check-out-repo
  (let [;; clear clone cache
        _ (io/rmdir-rf "tmp/git/cache")

        ;; create fake upstream repo (what would be on github
        ;; normally)
        d (.getCanonicalPath
           (jio/file
            (format "tmp/git/cache/test-%s" (str (clj-time.core/now)))))
        upstream (git/init-test-repo d)

        ;; create opts with the fake github in place
        opts1 (update-in
               (opts/parse-args ["-c" "test/runbld.yaml"
                                 "--job-name" "elastic,proj1,master"
                                 "/path/to/script.bash"])
               [:profiles :elastic-proj1-master :git]
               assoc :remote d)

        ;; now clone upstream to the cache tmp/git/cache/elastic/proj1
        opts2 ((build/wrap-build-meta
                (build/wrap-merge-profile
                 identity)) opts1)

        ;; clear out the workspace
        _ (io/rmdir-rf (get-in opts2 [:build :workspace]))

        ;; finally similate a build, where it does a fetch in the
        ;; cached clone and then clones to the workspace
        opts3 ((build/wrap-git-repo identity) opts2)]
    ;; (clojure.pprint/pprint {:opts3 opts2})
    (is (= 1 (count
              (jgit/git-log
               (jgit/load-repo
                (get-in opts3 [:build :workspace]))))))))
