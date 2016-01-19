(ns runbld.system
  (:require [runbld.schema :refer :all]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+]])
  (:require [clj-yaml.core :as yaml]
            [runbld.facts.factory :as facter]
            [runbld.fs.factory :as fs]
            [runbld.io :as io]))

(defmacro non-nil-proto-map [ns obj ks]
  `(apply merge
          (for [k# ~ks]
            (if-let [v# ((ns-resolve ~ns (symbol (name k#))) ~obj)]
              {k# v#}))))

(s/defn inspect-system :- BuildSystem
  ([facter fs]
   (merge
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
      :uptime
      :virtual])
    (non-nil-proto-map
     'runbld.fs fs
     [:fs-mountpoint
      :fs-type
      :fs-bytes-total
      :fs-bytes-free
      :fs-bytes-used
      :fs-percent-free
      :fs-percent-used])
    {})))

(s/defn wrap-system :- OptsWithSys
  [proc :- clojure.lang.IFn]
  (fn [opts]
    (proc (assoc opts :sys (inspect-system
                            (facter/make-facter)
                            (fs/make-fs (-> opts :process :cwd)))))))
