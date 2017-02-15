(ns runbld.notifications.slack
  (:refer-clojure :exclude [send])
  (:require [runbld.schema :refer :all]
            [schema.core :as s]
            [cheshire.core :as json]
            [clj-http.client :as http]
            [runbld.io :as io]
            [runbld.notifications :as n]
            [runbld.store :as store]
            [stencil.core :as mustache]
            [elasticsearch.document :as doc]
            [robert.bruce :refer [try-try-again] :as try]))

(defn api-send
  "Make the Slack REST API call"
  [opts js hook]
  (do
    ((opts :logger) "NOTIFYING SLACK")
    (try-try-again
     {:sleep 500 :tries 20 :decay try/linear}
     #(http/post hook
                 {:body js}))))

(s/defn send :- s/Any
  "Format and send the Slack notifcation"
  [opts :- MainOpts
   ctx  :- NotifyCtx]
  (let [f     (-> opts :slack :template)
        tmpl  (-> f io/resolve-resource slurp)
        color (if (-> ctx :process :failed)
                "danger"
                "good")
        js    (mustache/render-string tmpl (assoc ctx :color color))
        hooks (-> opts :slack :hook)]
    (if (string? hooks)
      (api-send opts js hooks)
      (doall (map #(api-send opts js %) hooks)))))

(defn first-successful-build?
  "If the build before this one was a failure, send a success notification to
  indicate that the build has been fixed."
  [opts build-doc]
  (when (-> opts :slack :first-success)
    (let [job (-> build-doc :build :job-name)
          query {:body
                 {:size 2
                  :query {:term {:build.job-name job}}
                  :sort {:process.time-end {:order "desc"}}}}
          previous (doc/search (-> opts :es :conn) query)]
      (-> previous
          :hits
          :hits
          second
          :_source
          :process
          :status
          (= "FAILURE")))))

(defn send-success? [opts build]
  (or (-> opts :slack :success)
      (first-successful-build? opts build)))

(defn send?
  "Determine whether to send a slack alert depending on configs"
  [opts build]
  (let [ec (-> build :process :exit-code)]
    (when-not (-> opts :slack :disable)
      (if (zero? ec)
        (send-success? opts build)
        (-> opts :slack :failure)))))

(defn maybe-send! [opts {:keys [index type id] :as addr}]
  (let [build-doc (store/get (-> opts :es :conn) addr)
        failure-docs (store/get-failures opts (:id build-doc))]
    (when (send? opts build-doc)
      (let [ctx (n/make-context opts build-doc failure-docs)]
        (try
          (send opts ctx)
          (catch Exception e
            ((opts :logger)
             (str "Caught exception while notifying Slack: "
                  (.getMessage e)))))))))
