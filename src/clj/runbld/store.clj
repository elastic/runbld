(ns runbld.store
  (:refer-clojure :exclude [get])
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [clojure.string :as str]
            [elasticsearch.document :as doc]
            [elasticsearch.indices :as indices]
            [elasticsearch.connection :as conn]
            [elasticsearch.connection.http :as http]
            [runbld.util.date :as date]
            [runbld.vcs :refer [VcsRepo]]
            [slingshot.slingshot :refer [try+ throw+]])
  (:import (elasticsearch.connection Connection)))

(def MAX_INDEX_BYTES (* 1024 1024 1024))

(defn make-connection
  [{:keys [url http-opts] :as args}]
  (http/make args))

(s/defn newest-index-matching-pattern :- (s/maybe s/Keyword)
  [conn pat]
  (->> (indices/get-settings conn pat)
       (map #(vector
              (key %)
              (Long/parseLong
               (get-in (val %) [:settings :index :creation_date]))))
       (sort-by second)
       reverse
       first
       first))

(s/defn index-size-bytes :- s/Num
  [conn :- Connection
   idx :- s/Keyword]
  (let [r (conn/request conn :get
                        {:uri (format "/%s/_stats/store" (name idx))})]
    (get-in r [:indices idx :primaries :store :size_in_bytes])))

(s/defn create-index :- s/Str
  [conn :- Connection
   idx :- s/Str
   body :- {s/Keyword s/Any}]
  (try+
   (indices/create conn idx {:body body})
   (catch [:status 400] e
     (when-not (= (get-in e [:body :error :type])
                  "index_already_exists_exception")
       (throw+ e))))
  idx)

(s/defn create-timestamped-index :- s/Str
  [conn :- Connection
   prefix :- s/Str
   body :- {s/Keyword s/Any}
   ]
  (let [idx (format "%s%d" prefix (System/currentTimeMillis))]
    (create-index conn idx body)))

(s/defn set-up-index :- s/Str
  ([conn idx body]
   (set-up-index conn idx body MAX_INDEX_BYTES))
  ([conn :- Connection
    idx :- s/Str
    body :- {(s/optional-key :mappings) {s/Any s/Any}
             (s/optional-key :settings) {s/Any s/Any}}
    max-bytes :- s/Num]
   (let [newest (newest-index-matching-pattern conn (format "%s*" idx))]
     (if (and newest (<= (index-size-bytes conn newest) max-bytes))
       (name newest)
       (create-timestamped-index conn (format "%s-" idx) body)))))

(s/defn create-build-doc :- StoredBuild
  [opts result test-report]
  (merge
   (select-keys opts [:id :version :system
                      :vcs :sys :jenkins :build])
   {:process (dissoc result :err-file :out-file)
    :test (when (:report-has-tests test-report)
            (let [f #(select-keys % [:error-type
                                     :class
                                     :test
                                     :type
                                     :message
                                     :summary])]
              (update (:report test-report)
                      :failed-testcases #(map f %))))}))

(s/defn save-build!
  [opts
   result
   test-report]
  (let [d (create-build-doc opts result test-report)
        conn (-> opts :es :conn)
        idx (-> opts :es :build-index-write)
        t (name DocType)
        id (:id d)
        es-addr {:index idx :type t :id id}]
    (doc/index conn (merge es-addr {:body d
                                    :query-params {:refresh true}}))
    {:url (format "%s://%s:%s/%s/%s/%s"
                  (-> opts :es :conn :settings :scheme name)
                  (-> opts :es :conn :settings :server-name)
                  (-> opts :es :conn :settings :server-port)
                  idx t id)
     :addr es-addr}))

(s/defn create-failure-docs :- [StoredFailure]
  [opts :- OptsFinal
   result :- ProcessResult
   failures :- [FailedTestCase]]
  (map #(assoc %
               :build-id (:id opts)
               :time (:time-end result)
               :org (-> opts :build :org)
               :project (-> opts :build :project)
               :branch (-> opts :build :branch)) failures))

(s/defn save-failures!
  [opts failures]
  (doseq [d failures]
    (let [conn (-> opts :es :conn)
          idx (-> opts :es :failure-index-write)
          t (name DocType)
          es-addr {:index idx :type t}]
      (doc/index conn (merge es-addr {:body d
                                      :query-params {:refresh true}})))))

(s/defn save! :- {s/Keyword s/Any}
  ([opts        :- OptsFinal
    result      :- ProcessResult
    test-report :- TestReport]
   (when (:report-has-tests test-report)
     ((opts :logger) (format "SAVE %d FAILURES" (count
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

(s/defn get :- StoredBuild
  ([conn addr]
   (:_source (doc/get conn addr))))

(s/defn get-failures
  ([opts id]
   (let [conn (-> opts :es :conn)
         idx (-> opts :es :failure-index-search)
         body {:query
               {:match
                {:build-id id}}}]
     (try+
      (->> (doc/search conn {:index idx :body body})
           :hits
           :hits
           (map :_source))
      (catch [:status 404] e
        [])))))
