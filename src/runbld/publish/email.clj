(ns runbld.publish.email
  (:refer-clojure :exclude [send])
  (:require [clojure.string :as str]
            [postal.core :as mail]
            [runbld.opts :refer [Opts]]
            [schema.core :as s]
            [stencil.core :as mustache]))

(defn maybe-split-addr [s]
  (if (and (string? s) (.contains s ","))
    (->> (str/split s #",")
         (map #(.trim %)))
    s))

(defn send* [conn from to subject msg]
  (mail/send-message
   conn
   {:from from
    :to to
    :subject subject
    :body msg}))

(s/defn send :- clojure.lang.IPersistentVector
  [opts :- Opts
   ctx :- {
           (s/required-key :commit) s/Str
           (s/required-key :commit-time)  s/Str
           (s/required-key :env) {s/Str s/Any}
           (s/required-key :github-name) s/Str
           (s/required-key :id) s/Str
           (s/required-key :job-name) s/Str
           (s/required-key :mail-from) s/Str
           (s/required-key :org) s/Str
           (s/required-key :project) s/Str
           (s/required-key :rcpt-to) (s/cond-pre s/Str [s/Str])
           (s/required-key :scriptfile) s/Str
           (s/required-key :start-millis) s/Num
           (s/required-key :time-end) s/Str
           (s/required-key :time-start) s/Str

           (s/optional-key :args) [s/Str]
           (s/optional-key :author-email) s/Str
           (s/optional-key :author-name) s/Str
           (s/optional-key :branch) s/Str
           (s/optional-key :cmd) [s/Str]
           (s/optional-key :cmd-source) s/Str
           (s/optional-key :cwd) s/Str
           (s/optional-key :end-millis) s/Num
           (s/optional-key :github-page) s/Str
           (s/optional-key :jenkins-executor) (s/maybe s/Str)
           (s/optional-key :jenkins-labels) (s/maybe s/Str)
           (s/optional-key :jenkins-node) (s/maybe s/Str)
           (s/optional-key :jenkins-number) (s/maybe s/Str)
           (s/optional-key :job-name-extra) s/Str
           (s/optional-key :profile-name) s/Str
           (s/optional-key :program) s/Str
           (s/optional-key :status) s/Num
           (s/optional-key :took) s/Num
           (s/optional-key :url) (s/maybe s/Str)
           (s/optional-key :workspace) s/Str
           }]
  (send* (opts :email)
         (ctx :mail-from)
         (ctx :rcpt-to)
         (format "%s %s"
                 (ctx :github-name)
                 (ctx :commit))
         (mustache/render-string
          (slurp (-> opts :email :template))
          ctx)))
