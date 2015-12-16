(ns runbld.publish.elasticsearch
  (:require [elasticsearch.document :as doc]
            [runbld.util.date :as date]))

(defn index [opts body]
  (assert (not (nil? (-> opts :es :index))))
  (let [doc {:index (-> opts :es :index)
             :type "b"
             :body body}]
    (doc/index (-> opts :es :conn) doc)))
