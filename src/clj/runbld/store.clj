(ns runbld.store
  (:refer-clojure :exclude [get])
  (:require [clojure.core.async :as async
             :refer [go go-loop chan >! <! alts! close!]]
            [clojure.future :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as st]
            runbld.spec
            [clojure.string :as str]
            [elasticsearch.document :as doc]
            [elasticsearch.indices :as indices]
            [elasticsearch.connection :as conn]
            [elasticsearch.connection.http :as http]
            [runbld.util.date :as date]
            [runbld.io :as io]
            [runbld.store.mapping :as m]
            [runbld.vcs :refer [VcsRepo]]
            [slingshot.slingshot :refer [try+ throw+]]))

(def MAX_INDEX_BYTES (* 1024 1024 1024))

(def MAX_TERM_LENGTH 32766)

(s/fdef make-connection
        :args (s/cat :m ::es-opts)
        :ret ::conn)
(defn make-connection
  [args]
  (http/make args))

(s/fdef newest-index-matching-pattern
        :args (s/cat :c ::conn :pat string?)
        :ret keyword?)
(defn newest-index-matching-pattern
  [conn pat]
  (try+
   (let [resp (indices/get-settings conn {:index pat})]
     (when (not-empty resp)
       (->> resp
            (map #(vector
                   (key %)
                   (Long/parseLong
                    (get-in (val %) [:settings :index :creation_date]))))
            (sort-by second)
            reverse
            first
            first
            name)))
   (catch [:status 404] _
     ;; index doesn't exist
     )))

(s/fdef index-size-bytes
        :args (s/cat :c ::conn :idx ::index-name)
        :ret int?)
(defn index-size-bytes
  [conn idx]
  (let [r (try+
           (conn/request conn :get
                         {:uri (format "/%s/_stats/store" (name idx))})
           (catch [:status 404] _ nil))
        size (get-in r [:indices idx :primaries :store :size_in_bytes])]
    (or size 0)))

(s/fdef create-index
        :args (s/cat :c ::conn :idx ::index-name :body map?)
        :ret ::index-name)
(defn create-index
  [conn idx body]
  (try+
   (indices/create conn idx {:body body})
   (indices/wait-for-health conn :yellow idx)
   (catch [:status 400] e
     (when-not (= (get-in e [:body :error :type])
                  "index_already_exists_exception")
       (throw+ e))))
  idx)

(s/fdef create-timestamped-index
        :args (s/cat :c ::conn
                     :p string?
                     :body ::index-meta))
(defn create-timestamped-index
  [conn prefix body]
  (let [idx (format "%s-%d" prefix (System/currentTimeMillis))]
    (create-index conn idx body)))

(s/fdef set-up-index
        :args (s/alt
               :a1 (s/cat :c ::conn
                          :i ::index-name
                          :b map?)
               :a2 (s/cat :c ::conn
                          :i ::index-name
                          :b map?
                          :bytes ::max-index-bytes))
        :ret keyword?)
(defn set-up-index
  ([conn idx body]
   (set-up-index conn idx body MAX_INDEX_BYTES))
  ([conn idx body max-bytes]
   (let [newest (newest-index-matching-pattern
                 conn (format "%s*" idx))]
     (if (and newest (<= (index-size-bytes conn newest) max-bytes))
       (name newest)
       (create-timestamped-index conn idx body)))))

(defn truncate-message [{:keys [message] :as doc}]
  (if message
    (let [bytes (.getBytes message "UTF-8")
          truncated (if (> (count bytes) MAX_TERM_LENGTH)
                      (String. (byte-array (take MAX_TERM_LENGTH bytes))
                               "UTF-8")
                      message)]
      (merge doc {:message truncated}))
    doc))

(s/fdef set-up-es!
          :args (s/cat :o ::opts)
          :ret int?)
(defn set-up-es! [{:keys [url
                          build-index
                          failure-index
                          log-index
                          max-index-bytes] :as opts}]
  (let [conn (make-connection
              (select-keys opts [:url :http-opts]))
        build-index-write (set-up-index
                           conn build-index
                           m/stored-build-index-settings
                           max-index-bytes)
        failure-index-write (set-up-index
                             conn failure-index
                             m/stored-failure-index-settings
                             max-index-bytes)
        log-index-write (set-up-index
                         conn log-index
                         m/stored-log-index-settings
                         max-index-bytes)]
    (-> opts
        (assoc :build-index-search (format "%s*" build-index))
        (assoc :failure-index-search (format "%s*" failure-index))
        (assoc :log-index-search (format "%s*" log-index))
        (assoc :build-index-write build-index-write)
        (assoc :failure-index-write failure-index-write)
        (assoc :log-index-write log-index-write)
        (assoc :conn conn))))

#_(s/fdef create-base-build-doc
          :args (s/cat :o ::opts)
          :ret ::build-doc-init)
(defn create-base-build-doc
  [opts]
  (select-keys opts [:id :version :system :java
                     :vcs :sys :jenkins :build]))

(defn create-build-doc
  [opts result test-report]
  (merge
   (create-base-build-doc opts)
   {:process (dissoc result :err-file :out-file)
    :test (when (:report-has-tests test-report)
            (let [f #(select-keys % [:error-type
                                     :class
                                     :test
                                     :type
                                     :message
                                     :summary])]
              (update (:report test-report)
                      :failed-testcases #(map (comp truncate-message f) %))))}))

(defn save-build!
  [opts
   result
   test-report]
  (let [d (create-build-doc opts result test-report)
        conn (-> opts :es :conn)
        idx (-> opts :es :build-index-write)
        t (name m/doc-type)
        id (:id d)
        es-addr {:index idx :type t :id id}]
    (doc/index conn idx t id {:body d
                              :query-params {:refresh true}})
    {:url (format "%s://%s:%s/%s/%s/%s"
                  (-> opts :es :conn :settings :scheme name)
                  (-> opts :es :conn :settings :server-name)
                  (-> opts :es :conn :settings :server-port)
                  idx t id)
     :addr es-addr
     :build-doc d}))

(defn create-failure-docs
  [opts result failures]
  (map #(assoc %
               :build-id (:id opts)
               :time (:time-end result)
               :org (-> opts :build :org)
               :project (-> opts :build :project)
               :branch (-> opts :build :branch)) failures))

(defn save-failures!
  [opts failures]
  (doseq [d failures]
    (let [conn (-> opts :es :conn)
          idx (-> opts :es :failure-index-write)
          t (name m/doc-type)
          es-addr {:index idx :type t}]
      (doc/index conn idx t {:body d
                             :query-params {:refresh true}}))))

(defn save!
  ([opts result test-report]
   (when (:report-has-tests test-report)
     ((opts :logger) (format "FAILURES: %d"
                             (count
                              (-> test-report
                                  :report
                                  :failed-testcases))))
     (save-failures! opts
                     (create-failure-docs opts result
                                          (-> test-report
                                              :report
                                              :failed-testcases))))
   (let [res (save-build! opts result test-report)]
     ((opts :logger) (format "BUILD: %s" (:url res)))
     res)))

(defn get
  ([conn {:keys [index type id]}]
   (:_source (doc/get conn index type id))))

(defn get-failures
  ([opts id]
   (let [conn (-> opts :es :conn)
         idx (-> opts :es :failure-index-search)
         body {:query
               {:match
                {:build-id id}}}]
     (try+
      (->> (doc/search conn idx {:body body})
           :hits
           :hits
           (map :_source))
      (catch [:status 404] e
        [])))))

(defn save-log!
  ([opts line]
   (doc/index (:conn opts)
              (opts :log-index-write)
              (name m/doc-type)
              {:body line})))

(defn save-logs!
  ([opts docs]
   (when (seq docs)
     (let [make (fn [doc]
                  {:index {:source doc}})
           actions (map make docs)]
       (doc/bulk (:conn opts)
                 (opts :log-index-write)
                 (name m/doc-type)
                 {:body actions})))))

(defn after-log
  ([{:keys [conn log-index-write]}]
   (indices/refresh conn log-index-write)))

(defn count-logs
  ([opts log-type id]
   (let [conn (-> opts :es :conn)
         idx (-> opts :es :log-index-write)
         body {:body
               {:query
                {:bool
                 {:must
                  [{:match {:build-id id}}
                   {:match {:stream log-type}}]}}}}]
     (:count (indices/count conn idx body)))))

(defn make-bulk-logger
  ([opts]
   (let [BUFSIZE (-> opts :bulk-size)
         TIMEOUT_MS (-> opts :bulk-timeout-ms)
         ch (chan BUFSIZE)
         proc (go-loop [buf []]
                (let [timed-out (async/timeout TIMEOUT_MS)
                      [x select] (alts! [ch timed-out])
                      newbuf
                      (cond
                        ;; got nil from the message chan, index buf,
                        ;; then exit
                        (and (nil? x) (= select ch))
                        (do
                          (save-logs! opts buf)
                          :die)

                        ;; have a value, and makes buf full
                        (and (not (nil? x))
                             (>= (inc (count buf)) BUFSIZE))
                        (do (save-logs! opts (conj buf x))
                            [])

                        ;; have a value, buf still not full
                        (and (not (nil? x))
                             (< (inc (count buf)) BUFSIZE))
                        (conj buf x)

                        ;; timer expired, index what's there
                        (and (= select timed-out)
                             (seq buf))
                        (do (save-logs! opts buf)
                            [])

                        ;; keep waiting
                        :else
                        buf)]
                  (when (not= newbuf :die)
                    (recur newbuf))))]
     [ch proc])))

#_(defn begin-process! [opts]
    )

#_(s/fdef begin-process!
          :args (s/cat :opts :runbld.opts/full)
          :ret keyword?)
