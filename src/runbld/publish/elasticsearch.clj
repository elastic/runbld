(ns runbld.publish.elasticsearch
  (:require [elasticsearch.document :as doc]
            [runbld.util.date :as date]))

(defn index [opts body]
  (assert (not (nil? (-> opts :opts :es.index.build))))
  (let [doc {:index (-> opts :opts :es.index.build)
             :type "b"
             :body body}]
    (doc/index (:es.conn opts) doc)))
