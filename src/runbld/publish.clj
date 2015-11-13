(ns runbld.publish
  (:require [runbld.publish.elasticsearch :as elasticsearch]
            [runbld.publish.email :as email]
            [schema.core :as s]
            [slingshot.slingshot :refer [try+ throw+]]))

(defn handlers []
  [#'elasticsearch/index
   #'email/send])

(defn make-context
  "Take all the useful stuff that's built up over the execution and
  trim it down to a view context. Anything here is fair game to be
  published."
  [opts]
  (merge
   (select-keys
    (:facter opts)
    [:architecture
     :hardwaremodel
     :hostname
     :ipaddress
     :ipaddress6
     :kernelrelease
     :kernelversion
     :memorysize_mb
     :operatingsystem
     :operatingsystemrelease
     :processor0
     :processorcount
     :physicalprocessorcount
     :timezone
     :uptime_days])
   (:process opts)
   (:build opts)
   {:mail-from (-> opts :email :from)
    :rcpt-to (email/split-addr (-> opts :email :to))
    :env (:env opts)}))

(defn publish* [errors f opts ctx]
  (try+
   (f opts ctx)
   (catch [:type :schema.core/error] {:keys [error] :as e}
     (throw+ {:error :validation :keys (keys e)}))
   (catch Throwable e
     (swap! errors conj e)
     :error)))

(defn publish [opts]
  (let [ctx (make-context opts)
        outputs (atom [])]
    (doseq [f (handlers)]
      (swap! outputs conj
             {:handler (.substring (str f) 2 (count (str f)))
              :output (publish* (:errors opts) f opts ctx)}))
    (merge
     opts
     {:publish
      {:context ctx
       :outputs @outputs}})))

(defn wrap-publish [proc]
  (fn [opts]
    (let [res (proc opts)]
      (publish res))))
