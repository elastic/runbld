(ns runbld.store
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [elasticsearch.document :as doc]
            [runbld.util.date :as date]
            [runbld.vcs :refer [VcsRepo]]
            [schema.core :as s])
  (:import (elasticsearch.connection Connection)))

(defn create-doc [opts result]
  (merge
   (select-keys opts [:id :system :vcs :sys :jenkins])
   {:process result}))

(s/defn ^:always-validate save! :- {s/Keyword s/Any}
  ([opts   :- OptsFinal
    result :- ProcessResult]
   (let [d (create-doc opts result)
         conn (-> opts :es :conn)
         idx (-> opts :es :index)]
     (doc/index conn {:index idx
                      :type "b"
                      :id (:id opts)
                      :body d}))))


