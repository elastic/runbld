(ns runbld.notifications.slack
  (:refer-clojure :exclude [send])
  (:require [runbld.schema :refer :all]
            [schema.core :as s]
            [cheshire.core :as json]
            [clj-http.client :as http]
            [runbld.io :as io]
            [runbld.notifications :as n]
            [runbld.store :as store]
            [stencil.core :as mustache]))

(s/defn send :- s/Any
  [opts :- MainOpts
   ctx  :- NotifyCtx]
  (let [f    (-> opts :slack :template)
        tmpl (-> f io/resolve-resource slurp)
        js   (mustache/render-string tmpl ctx)]
    (http/post (-> opts :slack :hook)
               {:body js})))

(defn send?
  "Send a slack alert depending on configs"
  [opts build]
  (let [ec (-> build :process :exit-code)]
    (or
     (and
      (zero? ec)
      (-> opts :slack :success))
     (and
      (pos? ec)
      (-> opts :slack :failure)))))

(defn maybe-send! [opts {:keys [index type id] :as addr}]
  (let [build-doc (store/get (-> opts :es :conn) addr)
        failure-docs (store/get-failures opts (:id build-doc))]
    (if (send? opts build-doc)
      (let [ctx (n/make-context opts build-doc failure-docs)]
        (send opts ctx )))))
