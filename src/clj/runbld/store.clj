(ns runbld.store
  (:refer-clojure :exclude [get])
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [elasticsearch.document :as doc]
            [elasticsearch.indices :as indices]
            [runbld.util.date :as date]
            [runbld.vcs :refer [VcsRepo]]
            [slingshot.slingshot :refer [try+]]))

(def doc-type
  "t")

(defn create-build-mappings [logger conn idx mappings]
  (let [body {:mappings {doc-type mappings}}]
    (try+
      (indices/create conn idx body)
      (catch [:status 400] e
        #_(logger idx "already exists")))))

(defn create-mappings [opts]
  (create-build-mappings
   (-> opts :logger)
   (-> opts :es :conn)
   (-> opts :es :build-index)
   StoredBuildMapping))

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
        idx (-> opts :es :build-index)
        t doc-type
        id (:id d)
        es-addr {:index idx :type t :id id}]
    (doc/index conn (merge es-addr {:body d}))
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
          idx (-> opts :es :failure-index)
          t doc-type
          es-addr {:index idx :type t}]
      (doc/index conn (merge es-addr {:body d})))))

(s/defn save! :- {s/Keyword s/Any}
  ([opts        :- OptsFinal
    result      :- ProcessResult
    test-report :- TestReport]
   (when (:report-has-tests test-report)
     (save-failures! opts
                     (create-failure-docs opts result
                                          (-> test-report
                                              :report
                                              :failed-testcases))))
   (save-build! opts result test-report)))

(s/defn get :- StoredBuild
  ([conn addr]
   (:_source (doc/get conn addr))))

(s/defn get-failures
  ([conn idx id]
   (let [body {:query
               {:match
                {:build-id id}}}]
     (try+
      (->> (doc/search conn {:index idx :body body})
           :hits
           :hits
           (map :_source))
      (catch [:status 404] e
        [])))))
