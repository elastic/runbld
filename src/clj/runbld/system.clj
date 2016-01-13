(ns runbld.system
  (:require [runbld.schema :refer :all]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+]])
  (:require [clj-yaml.core :as yaml]
            [runbld.facts :as facts]
            [runbld.facts.facter1]
            [runbld.facts.facter2]
            [runbld.facts.facter3]
            [runbld.io :as io]))

(def facter-program
  "facter")

(defn facter-installed? []
  (let [{:keys [exit]} (io/run "which" facter-program)]
    (zero? exit)))

(defn facter-version []
  (when (facter-installed?)
    (let [version-string (:out (io/run facter-program "--version"))
          [_ maj minor patch] (re-find #"(\d+)\.(\d+)\.(\d+).*"
                                       (.trim version-string))]
      (if (and maj minor patch)
        (zipmap
         [:major :minor :patch]
         (map #(Integer/parseInt %) [maj minor patch]))
        (throw+ {:error ::facter-version-unknown
                 :msg (format "don't understand facter version [%s]"
                              version-string)})))))

(defn facter []
  (if (facter-installed?)
    (let [{:keys [out err exit]} (io/run facter-program "--yaml")]
      (if out
        (yaml/parse-string out)
        (throw+ {:error ::empty-facter
                 :msg (format "facter returned empty (%d): %s" exit err)})))
    (throw+ {:warning ::no-facter
             :msg "facter cannot be found in PATH"})))

(defmacro non-nil-fact-map [ns facter ks]
  `(apply merge
          (for [k# ~ks]
            (if-let [v# ((ns-resolve ~ns (symbol (name k#))) ~facter)]
              {k# v#}))))

(s/defn inspect-system :- BuildSystem
  ([facter]
   (non-nil-fact-map
    'runbld.facts facter
    [:arch
     :cpu-type
     :cpus
     :cpus-physical
     :facter-provider
     :facter-version
     :hostname
     :ip4
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
     :virtual])))

(defn make-facter []
  (if-let [{:keys [major]} (facter-version)]
    (condp = major
      3 (runbld.facts.facter3.Facter3. (facter))
      2 (runbld.facts.facter2.Facter2. (facter))
      1 (runbld.facts.facter1.Facter1. (facter)))))

(s/defn wrap-system :- OptsWithSys
  [proc :- clojure.lang.IFn]
  (fn [opts]
    (proc (assoc opts :sys (inspect-system (make-facter))))))
