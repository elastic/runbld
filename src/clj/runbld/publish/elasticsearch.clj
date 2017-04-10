(ns runbld.publish.elasticsearch
  (:require [elasticsearch.document :as doc]
            [runbld.util.date :as date]))

(defn index [opts body]
  (assert (not (nil? (-> opts :es :index))))
  (doc/index (-> opts :es :conn)
             (-> opts :es :index)
             "b"
             {:body body}))
