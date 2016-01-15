(ns runbld.scheduler.middleware
  (:require [runbld.schema :refer :all]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+]])
  (:require [runbld.scheduler :as scheduler]
            [runbld.scheduler.jenkins :as jenkins]
            [runbld.scheduler.default :as default]))

(s/defn make-scheduler :- (s/protocol scheduler/Scheduler)
  [opts]
  (cond
    (not
     (nil?
      (get-in opts [:env :JENKINS_HOME]))) (jenkins/make opts)

    :else (default/make opts)))

(s/defn wrap-scheduler :- OptsWithScheduler
  [proc]
  (fn [opts]
    (proc
     (assoc opts :scheduler (make-scheduler opts)))))
