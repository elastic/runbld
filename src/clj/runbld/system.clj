(ns runbld.system
  (:require
   [clj-yaml.core :as yaml]
   [robert.bruce :refer [*last-try* try-try-again]]
   [runbld.facts :as facts]
   [runbld.facts.factory :as facter]
   [runbld.fs.factory :as fs]
   [runbld.hosting.factory :as hosting]
   [runbld.io :as io]
   [runbld.schema :refer :all]
   [schema.core :as s]
   [slingshot.slingshot :refer [throw+]]))

(defmacro non-nil-proto-map
  "Walk through and execute the given protocol fns, dropping if nil"
  [ns obj ks]
  `(apply merge
          (for [k# ~ks]
            (if-let [v# ((ns-resolve ~ns (symbol (name k#))) ~obj)]
              {k# v#}))))

(s/defn make-facts
  ([]
   (make-facts (facter/make-facter)))
  ([facter]
   (non-nil-proto-map
    'runbld.facts facter
    [:arch
     :cpu-type
     :cpus
     :cpus-physical
     :facter-provider
     :facter-version
     :hostname
     :ip4
     :ip6
     :kernel-name
     :kernel-release
     :kernel-version
     :model
     :os
     :os-version
     :ram-mb
     :ram-gb
     :timezone
     :uptime-days
     :uptime-secs
     :uptime])))

(s/defn make-fs
  ([dir]
   (non-nil-proto-map
    'runbld.fs (fs/make-fs dir)
    [:fs-mountpoint
     :fs-type
     :fs-bytes-total
     :fs-bytes-free
     :fs-bytes-used
     :fs-percent-free
     :fs-percent-used])))

(s/defn make-hosting
  ([facts]
   (non-nil-proto-map
    'runbld.hosting (hosting/make-hosting facts)
    [:datacenter
     :image-id
     :instance-id
     :instance-type
     :provider
     :region])))

(defn report-retry [err]
  (io/log
   (format "Error while gathering system facts. %s (Error was: %s)"
           (if-not *last-try*
             "Retrying."
             "Not retrying - out of attempts.")
           (.getMessage err)))
  ;; explicitly return nil so tries continue accordingly
  nil)

(s/defn inspect-system :- BuildSystem
  [cwd :- s/Str]
  (try-try-again
   {:sleep 5000
    :tries 5
    :catch java.lang.Throwable
    :error-hook report-retry}
   #(let [facter (facter/make-facter)]
      (merge
       (make-facts facter)
       (make-fs cwd)
       (make-hosting facter)))))

(s/defn add-system-facts :- OptsWithSys
  [opts :- Opts]
  (assoc opts :sys (inspect-system
                    (-> opts :process :cwd))))
