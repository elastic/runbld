(ns runbld.build
  (:require [runbld.util.date :as date]))

(defn make-rand-uuid []
  (.replaceAll (str (java.util.UUID/randomUUID)) "-" ""))

(defn make-id []
  (format "%s-%s"
          (date/yyyymmdd-hhmmss)
          (make-rand-uuid)))

(defn wrap-build-meta [proc]
  (fn [opts]
    (proc (assoc opts :id (make-id)))))
