(ns runbld.store
  (:require [elasticsearch.document :as doc]
            [runbld.util.date :as date]
            [runbld.vcs :refer [VcsRepo]]
            [schema.core :as s])
  (:import (elasticsearch.connection Connection)
           (runbld.host Host)))

(def StoredBuild
  {(s/required-key :id) s/Str
   (s/required-key :host) Host
   (s/required-key :repo) VcsRepo

   })

(s/defn ^:always-validate save!
  ([conn ]
   ))

(defn wrap-save [proc]
  (fn [opts]
    ))
