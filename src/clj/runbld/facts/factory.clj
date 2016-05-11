(ns runbld.facts.factory
  (:require [runbld.schema :refer :all]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+]])
  (:require [clj-yaml.core :as yaml]
            [runbld.facts.facter1]
            [runbld.facts.facter2]
            [runbld.facts.facter3]
            [runbld.io :as io]))

(def facter-program
  "facter")

(defn facter-installed? []
  (io/which facter-program))

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
  (when (facter-installed?)
    (let [{:keys [out err exit]} (io/run facter-program "--yaml")]
      (if out
        (yaml/parse-string out)
        (throw+ {:error ::empty-facter
                 :msg (format "facter returned empty (%d): %s" exit err)})))))

(defn make-facter []
  (if-let [{:keys [major]} (facter-version)]
    (condp = major
      3 (runbld.facts.facter3.Facter3. (facter))
      2 (runbld.facts.facter2.Facter2. (facter))
      1 (runbld.facts.facter1.Facter1. (facter)))))
