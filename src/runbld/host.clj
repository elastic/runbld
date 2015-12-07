(ns runbld.host
  (:require [clj-yaml.core :as yaml]
            [clojure.java.shell :as sh]
            [environ.core :as environ]
            [runbld.opts :as opts]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+]]))

(s/defrecord Host
    [
     arch           :- s/Str
     cpu-type       :- s/Str
     cpus           :- s/Num
     cpus-physical  :- s/Num
     hostname       :- s/Str
     ipv4           :- s/Str
     ipv6           :- s/Str
     kernel-release :- s/Str
     kernel-version :- s/Str
     model          :- s/Str
     os             :- s/Str
     os-version     :- s/Str
     ram-mb         :- s/Num
     timezone       :- s/Str
     uptime-secs    :- s/Int
     virtual        :- s/Bool
     ])

(defn facter-installed? []
  (let [{:keys [exit]} (sh/sh "which" "facter")]
`    (zero? exit)))

(defn facter []
  ;; costly call, only do it in production
  (if (environ/env :dev)
    (assoc (clojure.edn/read-string
            (slurp "test/facter.edn"))
           :dev-profile true)
    (if (facter-installed?)
      (yaml/parse-string (:out (sh/sh "facter" "--yaml")))
      (throw+ {:warning ::no-facter
               :msg "facter cannot be found in PATH"}))))

(s/defn inspect-host :- Host
  ([facter-fn :- clojure.lang.IFn]
   (let [facts (facter-fn)]
     (clojure.pprint/pprint facts)
     (map->Host
      {
       :arch           (:architecture            facts)
       :cpu-type       (:processor0              facts)
       :cpus           (:processorcount          facts)
       :cpus-physical  (:physicalprocessorcount  facts)
       :hostname       (:hostname                facts)
       :ipv4           (:ipaddress               facts)
       :ipv6           (:ipaddress6              facts)
       :kernel-release (:kernelrelease           facts)
       :kernel-version (:kernelversion           facts)
       :model          (:hardwaremodel           facts)
       :os             (:operatingsystem         facts)
       :os-version     (:operatingsystemrelease  facts)
       :ram-mb         (int
                        (Float/parseFloat
                         (:memorysize_mb facts)))
       :timezone       (:timezone                facts)
       :uptime-secs    (:uptime_seconds          facts)
       :virtual        (:is_virtual              facts)
       }))))

(defn wrap-host [proc]
  (fn [opts]
    (proc (assoc opts :sys (inspect-host facter)))))
