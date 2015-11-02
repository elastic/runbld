(ns runbld.publish
  (:require [elasticsearch.document :as doc]
            [runbld.util.date :as date]))

(defn expand-index-name [s]
  (if (.contains s "'")
    (date/expand s)
    s))

(defn make-doc [opts]
  {;; don't use this for :_id so we can take advantage of flake
   :id (get-in opts [:id])
   :status (get-in opts [:proc :status])})

(defn prepare-opts [opts]
  (assoc opts
         :es {:index (expand-index-name (:es.index.build opts))
              :type "b"
              :body (make-doc opts)}))

(defn index [opts]
  (doc/index (:es.conn opts) (:es opts)))

(defn publish [opts]
  (let [opts* (prepare-opts opts)]
    (index opts*)
    opts*))

(defn wrap-publish [proc]
  (fn [opts]
    (let [res (proc opts)]
      (publish res))))
