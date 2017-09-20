(ns runbld.facts.facter2
  (:require
   [runbld.facts :refer [Facter] :as facts]
   [runbld.util.data :as data]))

(defrecord Facter2 [facts]
  Facter

  (raw [x]
    (.facts x))

  (arch [x]
    (-> (.facts x) :architecture))

  (cpu-type [x]
    (-> (.facts x) :processor0))

  (cpus [x]
    (-> (.facts x) :processorcount))

  (cpus-physical [x]
    (-> (.facts x) :physicalprocessorcount))

  (facter-provider [x]
    "facter")

  (facter-version [x]
    (-> (.facts x) :facterversion))

  (hostname [x]
    (-> (.facts x) :hostname))

  (ip4 [x]
    (-> (.facts x) :ipaddress))

  (ip6 [x]
    (-> (.facts x) :ipaddress6))

  (kernel-name [x]
    (-> (.facts x) :kernel))

  (kernel-release [x]
    (-> (.facts x) :kernelrelease))

  (kernel-version [x]
    (-> (.facts x) :kernelversion))

  (model [x]
    (-> (.facts x) :hardwaremodel))

  (os [x]
    (-> (.facts x) :operatingsystem))

  (os-version [x]
    (-> (.facts x) :operatingsystemrelease))

  (ram-mb [x]
    (Float/parseFloat
     (-> (.facts x) :memorysize_mb)))

  (ram-gb [x]
    (data/bigdec (/ (facts/ram-mb x) 1024) 2))

  (ram-bytes [x]
    nil)

  (timezone [x]
    (-> (.facts x) :timezone))

  (uptime-days [x]
    (-> (.facts x) :uptime_days))

  (uptime-secs [x]
    (-> (.facts x) :uptime_seconds))

  (uptime [x]
    (-> (.facts x) :uptime))

  (virtual [x]
    (boolean
     (-> (.facts x) :is_virtual))))
