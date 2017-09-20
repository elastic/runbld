(ns runbld.hosting.hetzner
  (:require
   [runbld.hosting :refer [HostingProvider]]
   [runbld.io :as io]
   [runbld.schema :refer :all]
   [runbld.util.cidr :as cidr]
   [schema.core :as s]
   [slingshot.slingshot :refer [try+ throw+]]))

(def ip-ranges
  ["144.76.0.0/16"
   "5.9.0.0/16"])

(s/defn this-host?
  ([ip4]
   (->> ip-ranges
        (map (partial cidr/in-range? ip4))
        (some true?)
        boolean)))

(s/defn discover-datacenter
  "Based on
   http://wiki.hetzner.de/index.php/Rechenzentren_und_Anbindung/en,
   the DC name is always present in the perimeter Juniper
   hostname. Try looking for it."
  ([]
   (let [{:keys [out]} (io/run "traceroute" "-m 5" "google.com")]
     (discover-datacenter out)))
  ([trace]
   (let [[_ dc] (re-find #"(rz[0-9]+)\.hetzner\.de" trace)]
     dc)))

(defrecord HetznerHosting [facts]
  HostingProvider
  (datacenter [_]
    (discover-datacenter))

  (image-id [_])

  (instance-id [_])

  (instance-type [_])

  (provider [_]
    "hetzner")

  (region [_]))

(s/defn make
  ([facts]
   (HetznerHosting. facts)))
