(ns runbld.system
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [clj-yaml.core :as yaml]
            [clojure.java.shell :as sh]
            [environ.core :as environ]
            [runbld.opts :as opts]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+]])
  (:import (runbld.schema BuildSystem)))

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

(s/defn inspect-system :- BuildSystem
  ([facter-fn :- clojure.lang.IFn]
   (let [facts (facter-fn)]
     (map->BuildSystem
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

(defn wrap-system [proc]
  (fn [opts]
    (proc (assoc opts :sys (inspect-system facter)))))
