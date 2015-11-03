(ns runbld.publish.elasticsearch
  (:require [elasticsearch.document :as doc]
            [runbld.util.date :as date]))

(defn expand-index-name [s]
  (if (.contains s "'")
    (date/expand s)
    s))

(defn make-doc [opts]
  (merge
   (dissoc (:proc opts) :proc)
   (dissoc (:build opts) :id)
   {:processors      (get-in opts [:facter :processorcount])
    :timezone        (get-in opts [:facter :timezone])
    :hostname        (get-in opts [:facter :hostname])
    :architecture    (get-in opts [:facter :architecture])
    :hardwaremodel   (get-in opts [:facter :hardwaremodel])
    :operatingsystem (get-in opts [:facter :operatingsystem])
    :ipaddress       (get-in opts [:facter :ipaddress])
    :ipaddress6      (get-in opts [:facter :ipaddress6])
    :kernelrelease   (get-in opts [:facter :kernelrelease])
    :kernelversion   (get-in opts [:facter :kernelversion])
    :uptime_days     (get-in opts [:facter :uptime_days])
    :memorysize_mb   (get-in opts [:facter :memorysize_mb])
    }
   {
    ;; don't use this for :_id so we can take advantage of flake
    :id (get-in opts [:build :id])}
   ))

(defn prepare-opts [opts]
  (assoc opts
         :es {:index (expand-index-name (-> opts :opts :es.index.build))
              :type "b"
              :body (make-doc opts)}))

(defn index [opts]
  (doc/index (:es.conn opts) (:es opts)))
