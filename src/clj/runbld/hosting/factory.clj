(ns runbld.hosting.factory
  (:require [runbld.schema :refer :all]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+]])
  (:require [runbld.io :as io]
            [runbld.facts :as facts]
            [runbld.hosting :refer [HostingProvider]]
            [runbld.hosting.aws-ec2 :as ec2]
            [runbld.hosting.default :as default]
            [runbld.hosting.hetzner :as hetz]
            [runbld.util.data :as data]))

(s/defn make-hosting
  ([facter]
   (cond
     (hetz/this-host? (facts/ip4 facter)) (hetz/make facter)
     (ec2/this-host?)                     (ec2/make facter)
     :else                                (default/make facter))))
