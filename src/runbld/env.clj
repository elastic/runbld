(ns runbld.env
  (:import (org.yaml.snakeyaml Yaml))
  (:require [clojure.java.shell :as sh]
            [runbld.util.data :refer [keywordize-keys]]
            [slingshot.slingshot :refer [throw+]]))

(defn facter-installed? []
  (let [{:keys [exit]} (sh/sh "which" "facter")]
    (zero? exit)))

(defn facter []
  (if (facter-installed?)
    (->> (.load (Yaml.) (:out (sh/sh "facter" "--yaml")))
         (into {})
         keywordize-keys)
    (throw+ {:warning ::no-facter
             :msg "facter cannot be found in PATH"})))

(defn wrap-env [proc]
  (fn [opts]
    (proc (assoc opts
                 :env (into {} (System/getenv))
                 :facter (facter)))))
