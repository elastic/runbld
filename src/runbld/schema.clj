(ns runbld.schema
  (:require [schema.core :as s]
   ))

(s/defrecord EmailOpts
    [
     host               :- s/Str
     port               :- (s/cond-pre s/Num s/Str)
     tls                :- s/Bool
     user               :- s/Str
     pass               :- s/Str
     from               :- s/Str
     to                 :- (s/cond-pre s/Str [s/Str])
     template-txt       :- (s/cond-pre s/Str java.io.File)
     template-html      :- (s/cond-pre s/Str java.io.File)
     text-only          :- s/Bool
     max-failure-notify :- s/Num
     ])

#_(s/defrecord Opts
    [
     
     ]
  {(s/required-key :email)
   {(s/required-key :host) s/Str
    (s/required-key :port) (s/cond-pre s/Num s/Str)
    (s/optional-key :tls) s/Bool
    (s/optional-key :user) s/Str
    (s/optional-key :pass) s/Str
    (s/required-key :from) s/Str
    (s/required-key :to) (s/cond-pre s/Str [s/Str])
    (s/optional-key :template-txt) (s/cond-pre s/Str java.io.File)
    (s/optional-key :template-html) (s/cond-pre s/Str java.io.File)
    (s/optional-key :text-only) s/Bool
    (s/optional-key :max-failure-notify) s/Num}
   (s/required-key :env) {s/Str s/Str}
   (s/required-key :errors) clojure.lang.Atom
   (s/required-key :es) {s/Keyword s/Any}
   (s/required-key :git) {s/Keyword s/Any}
   (s/required-key :process) {s/Keyword s/Any}
   (s/required-key :build) {s/Keyword s/Any}
   (s/required-key :report) {s/Keyword s/Any}
   (s/optional-key :facter) {s/Keyword s/Any}
   (s/optional-key :profiles) {s/Keyword s/Any}
   (s/optional-key :version) (s/maybe s/Bool)
   (s/optional-key :config-file) (s/maybe s/Str)
   })
