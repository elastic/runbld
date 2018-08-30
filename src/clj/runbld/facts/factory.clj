(ns runbld.facts.factory
  (:require
   [clj-yaml.core :as yaml]
   [runbld.facts.facter3]
   [runbld.facts.oshi :as oshi]
   [runbld.io :as io]
   [runbld.schema :refer :all]
   [schema.core :as s]
   [slingshot.slingshot :refer [throw+]]))

(defn facter-version []
  (when-let [facter-bin (io/which "facter")]
    (let [version-string (:out (io/run facter-bin "--version"))
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
  (when-let [facter-bin (io/which "facter")]
    (let [{:keys [out err exit]} (io/run facter-bin "--yaml")]
      (if out
        (yaml/parse-string out)
        (throw+ {:error ::empty-facter
                 :msg (format "facter returned empty (%d): %s" exit err)})))))

(defn make-facter []
  (if-let [{:keys [major]} (facter-version)]
    (condp = major
      3 (runbld.facts.facter3.Facter3. (facter)))
    ;; No facter?  Try OSHI
    (runbld.facts.oshi.Oshi. (oshi/facts))))
