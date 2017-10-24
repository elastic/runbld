(ns runbld.hosting.factory
  (:require
   [runbld.facts :as facts]
   [runbld.hosting :refer [HostingProvider]]
   [runbld.hosting.aws-ec2 :as ec2]
   [runbld.hosting.default :as default]
   [runbld.hosting.hetzner :as hetz]
   [runbld.hosting.vagrant :as vagrant]
   [runbld.io :as io]
   [runbld.schema :refer :all]
   [runbld.util.data :as data]
   [schema.core :as s]
   [slingshot.slingshot :refer [throw+]]))

(s/defn make-hosting
  ([facter]
   (cond
     (ec2/this-host? facter)     (ec2/make facter)
     (hetz/this-host? facter)    (hetz/make facter)
     (vagrant/this-host? facter) (vagrant/make facter)
     :else                       (default/make facter))))
