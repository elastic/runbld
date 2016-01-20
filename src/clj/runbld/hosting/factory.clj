(ns runbld.hosting.factory
  (:require [runbld.schema :refer :all]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+]])
  (:require [runbld.io :as io]
            [runbld.facts :as facts]
            [runbld.hosting.aws-ec2 :as ec2]
            [runbld.hosting.default :as default]
            [runbld.hosting.hetzner :as hetz]
            [runbld.util.data :as data]))

(s/defn make-hosting
  ([facts]
   (cond
     (hetz/this-host?
      (facts/ip4 facts))             (hetz/make facts)

     (:ec2_metadata facts)           (ec2/make facts)

     :else                           (default/make facts))))
