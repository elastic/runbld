(ns runbld.notifications.slack
  (:refer-clojure :exclude [send])
  (:require
   [cheshire.core :as json]
   [clj-http.client :as http]
   [elasticsearch.document :as doc]
   [runbld.io :as io]
   [runbld.notifications :as n]
   [runbld.schema :refer :all]
   [runbld.store :as store]
   [runbld.util.http :refer [wrap-retries]]
   [schema.core :as s]
   [stencil.core :as mustache]))

(defn api-send
  "Make the Slack REST API call"
  [opts js hook]
  (do
    ((opts :logger) "NOTIFYING SLACK")
    (http/with-additional-middleware [wrap-retries]
      (http/post hook {:body js}))))

(defn render [{{:keys [template]} :slack :as opts}
              {{:keys [failed]} :process :as ctx}]
  (mustache/render-string
   (slurp (io/resolve-resource template))
   (assoc ctx :color (if failed "danger" "good"))))

(s/defn send :- s/Any
  "Format and send the Slack notifcation"
  [opts :- MainOpts
   ctx  :- NotifyCtx]
  (let [js (render opts ctx)
        hooks (get-in opts [:slack :hook])]
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
          previous (doc/search (-> opts :es :conn)
                               (-> opts :es :build-index-search) query)]
      (-> previous
          :hits
          :hits
          second
          :_source
          :process
          :status
          (= "FAILURE")))))

(defn send-success? [opts build]
  (if (-> opts :slack :first-success)
    (first-successful-build? opts build)
    (-> opts :slack :success)))

(defn send?
  "Determine whether to send a slack alert depending on configs"
  [opts build]
  (let [ec (-> build :process :exit-code)]
    (when-not (-> opts :slack :disable)
      (if (zero? ec)
        (send-success? opts build)
        (-> opts :slack :failure)))))

(defn maybe-send! [opts {:keys [index type id]}]
  (let [build-doc (store/get (-> opts :es :conn) index type id)
        failure-docs (store/get-failures opts (:id build-doc))]
    (when (send? opts build-doc)
      (let [ctx (n/make-context opts build-doc failure-docs)]
        (try
          (send opts ctx)
          (catch Exception e
            ((opts :logger)
             (str "Caught exception while notifying Slack: "
                  (.getMessage e)))))))))

(s/defn send-slack :- {:slack-result s/Any
                       s/Keyword s/Any}
  [opts :- {:store-result {:addr {s/Keyword s/Any}
                           :url s/Str
                           :build-doc {s/Keyword s/Any}}
            :slack OptsSlack
            s/Keyword s/Any}]
  (assoc opts :slack-result
         (io/try-log (maybe-send! opts (-> opts :store-result :addr)))))
