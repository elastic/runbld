(ns runbld.test.build-test
  (:require [clojure.test :refer :all]
            [runbld.opts :as opts])
  (:require [runbld.build :as build]
            :reload-all))

(deftest build-info
  (let [profiled-opts ((build/wrap-build-meta
                        (build/wrap-merge-profile
                         identity))
                       (opts/parse-args ["-c" "test/runbld.yaml"
                                         "--job-name" "elastic,proj1,master"
                                         "/path/to/script.bash"]))]
    (is (= "test/templates/proj1.mustache"
           (-> profiled-opts :email :template)))
    (is (= "foo@example.com"
           (-> profiled-opts :email :to)))))
