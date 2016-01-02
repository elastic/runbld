(ns runbld.scheduler.default
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [runbld.scheduler :refer [Scheduler] :as scheduler]))

(defrecord DefaultScheduler [opts]
  Scheduler
  (build-url [this] "default-build-url")
  (console-url [this]
    (format "%s/console" (scheduler/build-url this)))
  (tags [this] [])
  (extra-info [this] {})
  (provider [_] :default)
  (as-map [this]
    (merge
     {:scheduler (name (scheduler/provider this))
      :url (scheduler/build-url this)
      :console-url (scheduler/console-url this)
      :tags (scheduler/tags this)}
     (scheduler/extra-info this))))

(s/defn make :- (s/protocol Scheduler)
  [opts]
  (DefaultScheduler. opts))
