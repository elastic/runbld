(ns runbld.build-test
  (:require
   [clojure.test :refer :all]
   [runbld.build :as build]
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
