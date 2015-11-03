(ns runbld.publish
  (:require [runbld.publish.elasticsearch :as elasticsearch]
            [runbld.publish.email :as email]))

(defn publish [opts]
  (let [opts* (-> opts
                  elasticsearch/prepare-opts
                  email/prepare-opts)]
    (elasticsearch/index opts*)
    (email/send opts*)
    opts*))

(defn wrap-publish [proc]
  (fn [opts]
    (let [res (proc opts)]
      (publish res))))
