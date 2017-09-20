(ns runbld.scheduler.jenkins
  (:require
   [clojure.string :as str]
   [runbld.scheduler :refer [Scheduler] :as scheduler]
   [runbld.schema :refer :all]
   [runbld.util.data :refer [strip-trailing-slashes]]
   [schema.core :as s]))

(defrecord JenkinsScheduler [opts]
  Scheduler
  (build-url [this]
    (get-in (.opts this) [:env :BUILD_URL]))
  (console-url [this]
    (format "%s/console" (strip-trailing-slashes
                          (scheduler/build-url this))))
  (tags [this]
    (str/split
     (get-in (.opts this) [:env :NODE_LABELS]) #" "))
  (extra-info [this]
    (let [opts (.opts this)]
      {:number   (get-in opts [:env :BUILD_NUMBER])
       :executor (get-in opts [:env :EXECUTOR_NUMBER])
       :node     (get-in opts [:env :NODE_NAME])}))
  (provider [_] :jenkins)
  (as-map [this]
    (merge
     {:scheduler (name (scheduler/provider this))
      :url (scheduler/build-url this)
      :console-url (scheduler/console-url this)
      :tags (scheduler/tags this)}
     (scheduler/extra-info this))))

(s/defn make :- JenkinsScheduler
  [opts]
  (JenkinsScheduler. opts))
