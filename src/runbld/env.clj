(ns runbld.env
  (:require [clj-yaml.core :as yaml]
            [clojure.java.shell :as sh]
            [runbld.opts :as opts]
            [slingshot.slingshot :refer [throw+]]))

(defn facter-installed? []
  (let [{:keys [exit]} (sh/sh "which" "facter")]
    (zero? exit)))

(defn facter []
  ;; costly call, only do it in production
  (if opts/*dev*
    {:dev-profile true}
    (if (facter-installed?)
      (yaml/parse-string (:out (sh/sh "facter" "--yaml")))
      (throw+ {:warning ::no-facter
               :msg "facter cannot be found in PATH"}))))

(defn wrap-env [proc]
  (fn [opts]
    (proc (assoc opts
                 :env (into {} (System/getenv))
                 :facter (facter)))))
