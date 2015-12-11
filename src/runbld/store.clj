(ns runbld.store
  (:refer-clojure :exclude [get])
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [elasticsearch.document :as doc]
            [runbld.util.date :as date]
            [runbld.vcs :refer [VcsRepo]]
            [schema.core :as s]))

(s/defn create-doc :- StoredBuild
  [opts result]
  (merge
   (select-keys opts [:id :system :vcs :sys :jenkins])
   {:process result}))

(s/defn ^:always-validate save! :- {s/Keyword s/Any}
  ([opts   :- OptsFinal
    result :- ProcessResult]
   (let [d (create-doc opts result)
         conn (-> opts :es :conn)
         idx (-> opts :es :index)
         es-addr {:index idx
                  :type "b"
                  :id (:id opts)}]
     (doc/index conn (merge es-addr {:body d}))
     es-addr)))

(s/defn get :- StoredBuild
  ([conn addr]
   (:_source (doc/get conn addr))))
