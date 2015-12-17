(ns runbld.scheduler.jenkins
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [clojure.string :as str]
            [runbld.scheduler :refer [Scheduler] :as scheduler]))

(defrecord JenkinsScheduler [opts]
  Scheduler
  (build-url [this]
    (get-in (.opts this) [:env "BUILD_URL"]))
  (console-url [this]
    (format "%s/console" (scheduler/build-url this)))
  (tags [this]
    (str/split
     (get-in (.opts this) [:env "NODE_LABELS"]) " "))
  (extra-info [this]
    (let [opts (.opts this)]
      {:number   (get-in opts [:env "BUILD_NUMBER"])
       :executor (get-in opts [:env "EXECUTOR_NUMBER"])
       :node     (get-in opts [:env "NODE_NAME"])}))
  (vendor [_] :jenkins)
  (as-map [this]
    {(scheduler/vendor this) (scheduler/extra-info this)
     :scheduler-type (name (scheduler/vendor this))
     :url (scheduler/build-url this)
     :console-url (scheduler/console-url this)
     :tags (scheduler/tags this)}))

(s/defn make :- JenkinsScheduler
  [opts :- OptsStage4]
  (JenkinsScheduler. opts))
