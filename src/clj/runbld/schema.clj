(ns runbld.schema
  (:require [schema.core :as s]
            [runbld.scheduler :refer [Scheduler]])
  (:import (elasticsearch.connection Connection)))

(def VersionInfo
  {(s/required-key :string) s/Str
   (s/required-key :hash  ) s/Str})

(def OptsEmail
  {(s/required-key :from              ) s/Str
   (s/required-key :host              ) s/Str
   (s/required-key :max-failure-notify) s/Num
   (s/required-key :pass              ) s/Str
   (s/required-key :port              ) (s/cond-pre s/Num s/Str)
   (s/required-key :template-html     ) (s/cond-pre s/Str java.io.File)
   (s/required-key :template-txt      ) (s/cond-pre s/Str java.io.File)
   (s/required-key :text-only         ) s/Bool
   (s/required-key :tls               ) s/Bool
   (s/required-key :to                ) (s/cond-pre s/Str [s/Str])
   (s/required-key :user              ) s/Str})

(def OptsProcess
  {(s/required-key :program   ) s/Str
   (s/required-key :args      ) [s/Str]
   (s/required-key :cwd       ) s/Str
   (s/required-key :scriptfile) s/Str})

(def OptsElasticsearch
  {(s/required-key :build-index   ) s/Str
   (s/required-key :failure-index ) s/Str
   (s/required-key :conn          ) Connection
   (s/optional-key :http-opts     ) {s/Keyword s/Any}
   (s/optional-key :url           ) s/Str})

(def OptsS3
  {(s/required-key :access-key) s/Str
   (s/required-key :secret-key) s/Str
   (s/required-key :bucket    ) s/Str
   (s/required-key :prefix    ) s/Str})

(def Opts
  {(s/required-key :job-name    ) s/Str
   (s/required-key :version     ) VersionInfo
   (s/required-key :configfile  ) (s/maybe s/Str)
   (s/required-key :email       ) OptsEmail
   (s/required-key :es          ) OptsElasticsearch
   (s/required-key :process     ) OptsProcess
   (s/required-key :s3          ) OptsS3})

(def BuildSystem
  {(s/required-key :arch           ) s/Str
   (s/required-key :cpu-type       ) s/Str
   (s/required-key :cpus           ) s/Num
   (s/required-key :cpus-physical  ) s/Num
   (s/required-key :hostname       ) s/Str
   (s/required-key :ipv4           ) s/Str
   (s/optional-key :ipv6           ) s/Str
   (s/required-key :kernel-release ) s/Str
   (s/required-key :kernel-version ) s/Str
   (s/required-key :model          ) s/Str
   (s/required-key :os             ) s/Str
   (s/required-key :os-version     ) s/Str
   (s/required-key :ram-mb         ) s/Num
   (s/required-key :ram-gb         ) s/Num
   (s/required-key :timezone       ) s/Str
   (s/required-key :uptime         ) s/Str
   (s/required-key :uptime-days    ) s/Num
   (s/required-key :uptime-secs    ) s/Int
   (s/required-key :virtual        ) s/Bool})

(def Env
  {s/Str s/Any})

(def Build
  {(s/required-key :org                ) s/Str
   (s/required-key :project            ) s/Str
   (s/required-key :branch             ) s/Str
   (s/required-key :job-name-extra     ) s/Str
   (s/required-key :job-name           ) s/Str
   (s/required-key :org-project-branch ) s/Str
   (s/required-key :scheduler-type     ) s/Str
   (s/required-key :url                ) s/Str
   (s/required-key :console-url        ) s/Str
   (s/required-key :tags               ) [s/Str]
   (s/optional-key :number             ) s/Str
   (s/optional-key :executor           ) s/Str
   (s/optional-key :node               ) s/Str})

(def SchedulerInfo
  {(s/required-key :type     ) (s/maybe s/Str)
   (s/required-key :url      ) (s/maybe s/Str)
   (s/required-key :node     ) (s/maybe s/Str)})

(def OptsStage2
  (merge Opts {(s/required-key :sys) BuildSystem}))

(def OptsStage3
  (merge OptsStage2 {(s/required-key :env) Env}))

(def OptsStage4
  (merge OptsStage3 {(s/required-key :scheduler) (s/protocol Scheduler)}))

(def OptsStage5
  (merge OptsStage4 {(s/required-key :id) s/Str
                     (s/required-key :build) Build}))

(def OptsFinal
  (merge OptsStage5 {(s/required-key :vcs) {s/Keyword s/Any}}))

(def ProcessResult
  {(s/required-key :cmd            ) [s/Str]
   (s/required-key :cmd-source     ) s/Str
   (s/required-key :err-accuracy   ) s/Int
   (s/required-key :err-bytes      ) s/Num
   (s/required-key :err-file       ) s/Str
   (s/required-key :err-file-bytes ) s/Int
   (s/required-key :exit-code      ) s/Num
   (s/required-key :millis-end     ) s/Num
   (s/required-key :millis-start   ) s/Num
   (s/required-key :out-accuracy   ) s/Int
   (s/required-key :out-bytes      ) s/Num
   (s/required-key :out-file       ) s/Str
   (s/required-key :out-file-bytes ) s/Int
   (s/required-key :status         ) s/Str
   (s/required-key :time-end       ) s/Str
   (s/required-key :time-start     ) s/Str
   (s/required-key :took           ) s/Num})

(def StoreProcessResult
  {(s/required-key :cmd            ) [s/Str]
   (s/required-key :cmd-source     ) s/Str
   (s/required-key :err-accuracy   ) s/Int
   (s/required-key :err-bytes      ) s/Num
   (s/required-key :err-file-bytes ) s/Int
   (s/required-key :exit-code      ) s/Num
   (s/required-key :millis-end     ) s/Num
   (s/required-key :millis-start   ) s/Num
   (s/required-key :out-accuracy   ) s/Int
   (s/required-key :out-bytes      ) s/Num
   (s/required-key :out-file-bytes ) s/Int
   (s/required-key :status         ) s/Str
   (s/required-key :time-end       ) s/Str
   (s/required-key :time-start     ) s/Str
   (s/required-key :took           ) s/Num})

(def VcsLog
  {
   (s/required-key :author-name   ) s/Str
   (s/required-key :commit-id     ) s/Str
   (s/required-key :commit-short  ) s/Str
   (s/required-key :commit-time   ) s/Str
   (s/required-key :message       ) s/Str
   (s/required-key :type          ) s/Str
   (s/required-key :log-pretty    ) s/Str
   (s/required-key :project-url   ) s/Str
   (s/optional-key :branch-url    ) s/Str
   (s/optional-key :commit-url    ) s/Str

   (s/optional-key :author-email  ) s/Str
   (s/optional-key :author-time   ) s/Str
   (s/optional-key :commit-email  ) s/Str
   (s/optional-key :commit-name   ) s/Str
   (s/optional-key :message-full  ) s/Str
   })

(def XML
  {(s/required-key :tag     ) s/Keyword
   (s/required-key :attrs   ) {s/Keyword s/Str}
   (s/required-key :content ) [s/Any]})

(def FailedTestCase
  {(s/required-key :error-type ) s/Str
   (s/required-key :class      ) s/Str
   (s/required-key :test       ) s/Str
   (s/required-key :stacktrace ) s/Str
   (s/required-key :summary    ) s/Str
   (s/required-key :type       ) s/Str
   (s/optional-key :message    ) s/Str})

(def TestSummary
  {(s/required-key :errors    ) s/Num
   (s/required-key :failures  ) s/Num
   (s/required-key :tests     ) s/Num
   (s/required-key :skipped   ) s/Num
   (s/required-key :failed-testcases ) [FailedTestCase]})

(def TestReport
  {(s/required-key :report-has-tests) s/Bool
   (s/optional-key :report) TestSummary})

(def StoreTestSummary
  {(s/required-key :errors    ) s/Num
   (s/required-key :failures  ) s/Num
   (s/required-key :tests     ) s/Num
   (s/required-key :skipped   ) s/Num
   (s/required-key :failed-testcases)
   [{(s/required-key :error-type ) s/Str
     (s/required-key :class      ) s/Str
     (s/required-key :test       ) s/Str
     (s/required-key :type       ) s/Str
     (s/required-key :summary    ) s/Str
     (s/optional-key :message    ) s/Str}]})

(def StoredFailure
  (merge
   FailedTestCase
   {(s/required-key :build-id ) s/Str
    (s/required-key :time     ) s/Str
    (s/required-key :org      ) s/Str
    (s/required-key :project  ) s/Str
    (s/required-key :branch   ) s/Str}))

(def StoredBuild
  {(s/required-key :id     ) s/Str
   (s/required-key :version) VersionInfo
   (s/required-key :build  ) Build
   (s/required-key :sys    ) BuildSystem
   (s/required-key :vcs    ) VcsLog
   (s/required-key :process) StoreProcessResult
   (s/required-key :test   ) (s/maybe StoreTestSummary)})

(def EmailCtx
  {(s/required-key :id     ) s/Str
   (s/required-key :version) VersionInfo
   (s/required-key :build  ) Build
   (s/required-key :sys    ) BuildSystem
   (s/required-key :email  ) {(s/required-key :to) s/Str
                              (s/required-key :subject) s/Str
                              }
   (s/required-key :vcs    ) VcsLog
   (s/required-key :process) (merge StoreProcessResult
                                    {(s/required-key :cmd) s/Str
                                     (s/required-key :args) s/Str
                                     (s/required-key :took-human) s/Str})
   (s/required-key :test) (s/maybe StoreTestSummary)})
