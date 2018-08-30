(ns runbld.facts.facter3
  (:require
   [runbld.facts :refer [Facter] :as facts]
   [runbld.util.data :as data]))

(defrecord Facter3 [facts]
  Facter

  (raw [x]
    (.facts x))

  (arch [x]
    (-> (.facts x) :os :architecture))

  (cpu-type [x]
    (-> (.facts x) :processors :models first))

  (cpus [x]
    (-> (.facts x) :processors :count))

  (cpus-physical [x]
    (-> (.facts x) :processors :physicalcount))

  (facter-provider [x]
    "facter")

  (facter-version [x]
    (-> (.facts x) :facterversion))

  (hostname [x]
    (-> (.facts x) :networking :hostname))

  (ip4 [x]
    [(-> (.facts x) :networking :ip)])

  (ip6 [x]
    [(-> (.facts x) :networking :ip6)])

  (kernel-name [x]
    (-> (.facts x) :kernel))

  (kernel-release [x]
    (-> (.facts x) :kernelrelease))

  (kernel-version [x]
    (-> (.facts x) :kernelversion))

  (model [x]
    (-> (.facts x) :os :hardware))

  (os [x]
    (-> (.facts x) :os :name))

  (os-version [x]
    (-> (.facts x) :os :release :full))

  (os-family [x]
    (-> (.facts x) :os :family))

  (ram-mb [x]
    (data/bigdec
     (/ (facts/ram-bytes x) (* 1024 1024))
     2))

  (ram-gb [x]
    (data/bigdec
     (/ (facts/ram-bytes x) (* 1024 1024 1024))
     2))

  (ram-bytes [x]
    (-> (.facts x) :memory :system :total_bytes))

  (timezone [x]
    (-> (.facts x) :timezone))

  (uptime-days [x]
    (-> (.facts x) :system_uptime :days))

  (uptime-secs [x]
    (-> (.facts x) :system_uptime :seconds))

  (uptime [x]
    (-> (.facts x) :system_uptime :uptime))

  (virtual [x]
    (boolean
     (-> (.facts x) :is_virtual))))
