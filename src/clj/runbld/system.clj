(ns runbld.system
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [clj-yaml.core :as yaml]
            [clojure.java.shell :as sh]
            [clojure.java.io :as io]
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

(defn find-ram-mb
  "Hack until we can fix our facter versions, or get it to always
  return memory info"
  [facts]
  (if (:memorysize_mb facts)
    (Float/parseFloat
     (:memorysize_mb facts))
    (let [meminfo "/proc/meminfo"]
      (if (and (= (:kernel facts) "Linux")
               (.exists (io/file meminfo)))
        (let [memtotal-raw (:out
                            (sh/sh "fgrep" "MemTotal" meminfo))
              [_ kb] (or (re-find #"^MemTotal: +(\d+) kB" memtotal-raw)
                         (throw+ {:type ::error
                                  :msg (format
                                        "can't get memtotal from meminfo:\n%s"
                                        (with-out-str
                                          (println memtotal-raw)
                                          (clojure.pprint/pprint facts)))}))]
          (float (/ (Integer/parseInt kb) 1024)))
        (throw+ {:type ::error
                 :msg (format "can't get memory info from:\n%s"
                              (with-out-str
                                (clojure.pprint/pprint facts)))})))))

(defn memory-details [facts]
  (let [ram-mb (find-ram-mb facts)
        ram-gb (.setScale
                (bigdec (/ ram-mb 1024)) 2
                java.math.BigDecimal/ROUND_HALF_UP)]
    {:ram-mb ram-mb
     :ram-gb ram-gb}))

(defn as-int [value]
  (when value
    (cond
      (integer? value) value
      (and
       (string? value)
       (re-find #"\d+" value)) (Integer/parseInt value)
      :else (throw+
             {:type ::error
              :msg (format
                    "don't know how to make an integer out of: %s (%s)"
                    (pr-str value)
                    (type value))}))))

(s/defn inspect-system :- BuildSystem
  ([facter-fn :- clojure.lang.IFn]
   (let [facts (facter-fn)
         ipv6 (:ipaddress6 facts)]
     (merge
      {:arch           (:architecture            facts)
       :cpu-type       (:processor0              facts)
       :cpus           (as-int
                        (:processorcount         facts))
       :cpus-physical  (as-int
                        (:physicalprocessorcount facts))
       :hostname       (:hostname                facts)
       :ipv4           (:ipaddress               facts)
       :kernel-release (:kernelrelease           facts)
       :kernel-version (:kernelversion           facts)
       :model          (:hardwaremodel           facts)
       :os             (:operatingsystem         facts)
       :os-version     (:operatingsystemrelease  facts)
       :timezone       (:timezone                facts)
       :uptime-days    (:uptime_days             facts)
       :uptime-secs    (:uptime_seconds          facts)
       :uptime         (:uptime                  facts)
       :virtual        (boolean
                        (:is_virtual             facts))}
      (memory-details facts)
      (when ipv6
        {:ipv6 ipv6})))))

(s/defn wrap-system :- OptsStage2
  [proc :- clojure.lang.IFn]
  (fn [opts]
    (proc (assoc opts :sys (inspect-system facter)))))
