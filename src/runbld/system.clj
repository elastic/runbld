(ns runbld.system
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [clj-yaml.core :as yaml]
            [clojure.java.shell :as sh]
            [environ.core :as environ]
            [runbld.opts :as opts]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+]]))

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
   (let [facts (facter-fn)
         ram-mb (Float/parseFloat (:memorysize_mb facts))
         ram-gb (.setScale
                 (bigdec (/ ram-mb 1024)) 2
                 java.math.BigDecimal/ROUND_HALF_UP)]
     {:arch           (:architecture            facts)
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
      :ram-mb         ram-mb
      :ram-gb         ram-gb
      :timezone       (:timezone                facts)
      :uptime-secs    (:uptime_seconds          facts)
      :virtual        (:is_virtual              facts)})))

(s/defn wrap-system :- Opts2
  [proc :- clojure.lang.IFn]
  (fn [opts]
    (proc (assoc opts :sys (inspect-system facter)))))
