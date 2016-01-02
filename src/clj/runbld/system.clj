(ns runbld.system
  (:require [runbld.schema :refer :all]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+]])
  (:require [clj-yaml.core :as yaml]
            [runbld.facts :as facts]
            [runbld.facts.facter1]
            [runbld.facts.facter2]
            [runbld.facts.facter3]
            [runbld.util.io :as io]))

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

(s/defn inspect-system :- BuildSystem
  ([facter]
   (let [ip6 (facts/ip6 facter)
         ram-bytes (facts/ram-bytes facter)]
     (merge
      {:arch            (facts/arch            facter)
       :cpu-type        (facts/cpu-type        facter)
       :cpus            (facts/cpus            facter)
       :cpus-physical   (facts/cpus-physical   facter)
       :facter-provider (facts/facter-provider facter)
       :facter-version  (facts/facter-version  facter)
       :hostname        (facts/hostname        facter)
       :ip4             (facts/ip4             facter)
       :kernel-name     (facts/kernel-name     facter)
       :kernel-release  (facts/kernel-release  facter)
       :kernel-version  (facts/kernel-version  facter)
       :model           (facts/model           facter)
       :os              (facts/os              facter)
       :os-version      (facts/os-version      facter)
       :ram-mb          (facts/ram-mb          facter)
       :ram-gb          (facts/ram-gb          facter)
       :timezone        (facts/timezone        facter)
       :uptime-days     (facts/uptime-days     facter)
       :uptime-secs     (facts/uptime-secs     facter)
       :uptime          (facts/uptime          facter)
       :virtual         (facts/virtual         facter)}
      (when ip6
        {:ip6 ip6})
      (when ram-bytes
        {:ram-bytes ram-bytes})))))

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

