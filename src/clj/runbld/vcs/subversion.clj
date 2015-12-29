(ns runbld.vcs.subversion
  (:require [runbld.schema :refer :all]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+]]
            [clojure.reflect])
  (:require [runbld.util.date :as date]
            [runbld.vcs :as vcs :refer [VcsRepo]])
  (:import (runbld.vcs.subversion SvnRepository)))

(def vendor "svn")

(defn head-commit [this]
  (let [url (.url this)
        rev (.revision this)
        repo (SvnRepository. url)
        log (.latestLog repo rev rev)
        date (date/date-to-iso
              (.getDate log))
        msg (.getMessage log)
        author (.getAuthor log)
        revstr (str (.getRevision log))]
    {:author-name author
     :commit-id revstr
     :commit-short revstr
     :commit-time date
     :message msg
     :type vendor
     :log-pretty (format "revision: %s\nauthor: %s\ndate: %s\n\n%s"
                         (str (.getRevision log))
                         author
                         date
                         msg)}))

(defn branch-url
  [this]
  (format "%s?p=%d"
          (.url this)
          (.revision this)))

(defn project-url
  [this]
  (.url this))

(s/defn log-latest :- VcsLog
  ([this]
   (let [{:keys [commit-id] :as commit} (head-commit this)]
     (merge
      commit
      ;; TODO: ViewSVN support
      {:branch-url (branch-url this)
       :project-url (project-url this)}))))

(s/defrecord SvnRepo
    [url      :- s/Str
     org      :- s/Str
     project  :- s/Str
     revision :- s/Num])

(extend SvnRepo
  VcsRepo
  {:log-latest log-latest
   :vendor (fn [& args] vendor)})

(s/defn make-repo :- SvnRepo
  [url org project revision]
  (let [rev (if (string? revision)
              (Integer/parseInt revision)
              revision)]
    (SvnRepo. url org project rev)))
