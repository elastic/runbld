(ns runbld.publish
  (:require [runbld.publish.elasticsearch :as elasticsearch]
            [runbld.publish.email :as email]
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
     :processors
     :timezone
     :uptime_days])
   (dissoc (:proc opts) :proc)
   (:build opts)
   {:mail-from (-> opts :opts :email :from)
    :rcpt-to (-> opts :opts :email :to)
    :github-name (format "%s/%s#%s"
                         (-> opts :build :org)
                         (-> opts :build :project)
                         (-> opts :build :branch))
    :github-page (format "https://github.com/%s/%s/tree/%s"
                         (-> opts :build :org)
                         (-> opts :build :project)
                         (-> opts :build :branch))}))

(defn publish* [errors f opts ctx]
  (try
    (f opts ctx)
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
