(ns runbld.hosting.default
  (:require
   [runbld.hosting :refer [HostingProvider]]
   [runbld.schema :refer :all]
   [schema.core :as s]
   [slingshot.slingshot :refer [try+ throw+]]))

(defrecord DefaultHosting [facts]
  HostingProvider
  (datacenter    [_])

  (image-id      [_])

  (instance-id   [_])

  (instance-type [_])

  (provider [_]
    "default")

  (region        [_]))

(s/defn make
  ([facts]
   (DefaultHosting. facts)))
