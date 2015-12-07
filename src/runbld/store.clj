(ns runbld.store
  (:require [elasticsearch.document :as doc]
            [runbld.util.date :as date]
            [runbld.sys :refer [Sys]]
            [runbld.vcs :refer [VcsRepo]]
            [schema.core :as s])
  (:import [elasticsearch.connection Connection]))

(def StoredBuild
  {(s/required-key :id) s/Str
   (s/required-key :sys) Sys
   (s/required-key :repo) VcsRepo

   })

(s/defn ^:always-validate save!
  ([conn ]
   ))

(defn wrap-save [proc]
  (fn [opts]
    ))
