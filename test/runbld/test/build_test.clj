(ns runbld.test.build-test
  (:require [clojure.test :refer :all])
  (:require [runbld.build :as build]
            [runbld.opts :as opts]
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
  (let [opts1 (opts/parse-args ["-c" "test/runbld.yaml"
                                "--job-name" "elastic,proj1,master"
                                "/path/to/script.bash"])
        _    (clojure.pprint/pprint opts1)
        opts2 ((build/wrap-build-meta
                (build/wrap-merge-profile
                 (build/wrap-git-repo
                  identity))) opts1)]))
