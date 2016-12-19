(ns runbld.test.build-test
  (:require [clojure.test :refer :all]
            clojure.spec
            clojure.spec.test)
  (:require [runbld.build :as build]))

(clojure.spec.test/instrument)

(deftest basic
  (is
   (= {:branch "master"
       :job-name "elastic+elasticsearch+master+multijob-intake"
       :job-name-extra "multijob-intake"
       :org "elastic"
       :org-project-branch "elastic/elasticsearch#master"
       :project "elasticsearch"}
      (build/split-job-name
       "elastic+elasticsearch+master+multijob-intake"))))
