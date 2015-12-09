(ns runbld.schema
  (:require [schema.core :as s])
  (:import (elasticsearch.connection Connection)))

(def OptsEmail
  {
   (s/required-key :from              ) s/Str
   (s/required-key :host              ) s/Str
   (s/required-key :max-failure-notify) s/Num
   (s/required-key :pass              ) s/Str
   (s/required-key :port              ) (s/cond-pre s/Num s/Str)
   (s/required-key :template-html     ) (s/cond-pre s/Str java.io.File)
   (s/required-key :template-txt      ) (s/cond-pre s/Str java.io.File)
   (s/required-key :text-only         ) s/Bool
   (s/required-key :tls               ) s/Bool
   (s/required-key :to                ) (s/cond-pre s/Str [s/Str])
   (s/required-key :user              ) s/Str
   })

(def OptsProcess
  {
   (s/required-key :program   ) s/Str
   (s/required-key :args      ) [s/Str]
   (s/required-key :cwd       ) s/Str
   (s/required-key :scriptfile) s/Str
   })

(def OptsElasticsearch
  {
   (s/required-key :index    ) s/Str
   (s/required-key :conn     ) Connection
   (s/optional-key :http-opts) {s/Keyword s/Any}
   })

(def OptsS3
  {
   (s/required-key :access-key) s/Str
   (s/required-key :secret-key) s/Str
   (s/required-key :bucket    ) s/Str
   (s/required-key :prefix    ) s/Str
   })

(s/defrecord Opts
    [
     job-name    :- s/Str
     version     :- (s/maybe s/Bool)
     configfile  :- (s/maybe s/Str)
     email       :- OptsEmail
     es          :- OptsElasticsearch
     process     :- OptsProcess
     s3          :- OptsS3
     ]
  )

(s/defrecord BuildSystem
    [
     arch           :- s/Str
     cpu-type       :- s/Str
     cpus           :- s/Num
     cpus-physical  :- s/Num
     hostname       :- s/Str
     ipv4           :- s/Str
     ipv6           :- s/Str
     kernel-release :- s/Str
     kernel-version :- s/Str
     model          :- s/Str
     os             :- s/Str
     os-version     :- s/Str
     ram-mb         :- s/Num
     timezone       :- s/Str
     uptime-secs    :- s/Int
     virtual        :- s/Bool
     ])

(s/defrecord ProcessResult
    [
     cmd          :- [s/Str]
     cmd-source   :- s/Str
     err-accuracy :- s/Num
     err-bytes    :- s/Num
     err-file     :- s/Str
     exit-code    :- s/Num
     millis-end   :- s/Num
     millis-start :- s/Num
     out-accuracy :- s/Num
     out-bytes    :- s/Num
     out-file     :- s/Str
     status       :- s/Str
     time-end     :- s/Str
     time-start   :- s/Str
     took         :- s/Num
     ])

(s/defrecord VcsRepo
    [
     type :- s/Keyword
     ])

(s/defrecord StoredBuild
    [
     id :- s/Str
     system :- BuildSystem
     repo :- VcsRepo
     ])
