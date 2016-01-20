(ns runbld.hosting.default
  (:require [runbld.schema :refer :all]
            [schema.core :as s]
            [slingshot.slingshot :refer [try+ throw+]])
  (:require [runbld.hosting :refer [HostingProvider]]))

(defrecord DefaultHosting [facts]
  HostingProvider
  (datacenter    [_])

  (image-id      [_])

  (instance-id   [_])

  (instance-type [_])

  (hosting-provider [_]
    "default")

  (region        [_]))

(s/defn make
  ([facts]
   (DefaultHosting. facts)))
