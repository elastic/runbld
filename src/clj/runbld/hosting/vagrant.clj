(ns runbld.hosting.vagrant
  (:require
   [clojure.java.io :as jio]
   [runbld.hosting :refer [HostingProvider]]))

(defn this-host? [facts]
  (or (.exists (jio/file "/vagrant"))
      (.exists (jio/file "/home/vagrant"))
      (.exists (jio/file "/Users/vagrant"))))

(defrecord VagrantHosting [facts]
  HostingProvider
  (datacenter [_])
  (image-id [_])
  (instance-id [_])
  (instance-type [_])
  (provider [_] "vagrant")
  (region [_])
  (virtual [_] false))

(defn make [facts]
  (VagrantHosting. facts))
