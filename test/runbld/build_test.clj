(ns runbld.build-test
  (:require
   [clojure.test :refer :all]
   [runbld.build :as build]
   [schema.test :as s]))

(use-fixtures :once schema.test/validate-schemas)

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
