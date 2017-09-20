(ns runbld.util.http
  (:require
   [robert.bruce :refer [try-try-again]]))

(defn wrap-retries*
  [client {:keys [retries-config] :as req} & args]
  (try-try-again
   {:sleep (:sleep retries-config 500)
    :tries (:tries retries-config 20)}
   #(apply client (dissoc req :retries-config) args)))

(defn wrap-retries
  [client]
  (fn
    ([req]
     (wrap-retries* client req))
    ([req respond raise]
     (wrap-retries* client req respond raise))))
