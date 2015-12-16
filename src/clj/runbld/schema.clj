(ns runbld.schema
  (:require [schema.core :as s])
  (:import (elasticsearch.connection Connection)))

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
   (s/required-key :version     ) (s/maybe s/Bool)
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
   (s/required-key :ipv6           ) s/Str
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

(def BuildInfo
  {(s/required-key :org                ) s/Str
   (s/required-key :project            ) s/Str
   (s/required-key :branch             ) s/Str
   (s/required-key :job-name-extra     ) s/Str
   (s/required-key :org-project-branch ) s/Str})

(def JenkinsInfo
  {(s/required-key :url      ) (s/maybe s/Str)
   (s/required-key :number   ) (s/maybe s/Str)
   (s/required-key :executor ) (s/maybe s/Str)
   (s/required-key :node     ) (s/maybe s/Str)
   (s/required-key :labels   ) (s/maybe s/Str)})

(def Opts2
  (merge Opts {(s/required-key :sys) BuildSystem}))

(def Opts3
  (merge Opts2 {(s/required-key :env) Env}))

(def Opts4
  (merge Opts3 {(s/required-key :id) s/Str
                (s/required-key :build) BuildInfo
                (s/required-key :jenkins) JenkinsInfo}))

(def OptsFinal
  (merge Opts4 {(s/required-key :vcs) {s/Keyword s/Any}}))

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
   (s/required-key :build  ) BuildInfo
   (s/required-key :sys    ) BuildSystem
   (s/required-key :vcs    ) VcsLog
   (s/required-key :jenkins) java.util.Map
   (s/required-key :process) StoreProcessResult
   (s/required-key :test   ) (s/maybe StoreTestSummary)})

(def EmailCtx
  {(s/required-key :id     ) s/Str
   (s/required-key :build  ) BuildInfo
   (s/required-key :sys    ) BuildSystem
   (s/required-key :email  ) {(s/required-key :to) s/Str
                              (s/required-key :subject) s/Str
                              }
   (s/required-key :vcs    ) VcsLog
   (s/required-key :jenkins) JenkinsInfo
   (s/required-key :process) (merge StoreProcessResult
                                    {(s/required-key :cmd) s/Str
                                     (s/required-key :args) s/Str
                                     (s/required-key :took-human) s/Str})
   (s/required-key :test) (s/maybe StoreTestSummary)
   }

  #_{
     (s/required-key :branch-url       ) s/Str
     (s/required-key :build-name       ) s/Str
     (s/required-key :commit           ) s/Str
     (s/required-key :commit-desc      ) s/Str
     (s/required-key :commit-email     ) s/Str
     (s/required-key :commit-msg       ) s/Str
     (s/required-key :commit-name      ) s/Str
     (s/required-key :commit-time      ) s/Str
     (s/required-key :commit-url       ) s/Str
     (s/required-key :console-url      ) s/Str
     (s/required-key :env              ) {s/Str s/Any}
     (s/required-key :exit-code        ) s/Num
     (s/required-key :job-name         ) s/Str
     (s/required-key :out-bytes        ) s/Num
     (s/required-key :err-bytes        ) s/Num
     (s/required-key :out-accuracy     ) s/Num
     (s/required-key :err-accuracy     ) s/Num
     (s/required-key :out-file         ) s/Str
     (s/required-key :err-file         ) s/Str
     (s/required-key :mail-from        ) s/Str
     (s/required-key :org              ) s/Str
     (s/required-key :project          ) s/Str
     (s/required-key :project-url      ) s/Str
     (s/required-key :rcpt-to          ) [s/Str]
     (s/required-key :report           ) {(s/required-key :errors) s/Num
                                          (s/required-key :failures) s/Num
                                          (s/required-key :skipped) s/Num
                                          (s/required-key :tests) s/Num
                                          (s/required-key :testcases) [{s/Keyword s/Any}]}
     (s/required-key :scriptfile       ) s/Str
     (s/required-key :start-millis     ) s/Num
     (s/required-key :status           ) s/Str
     (s/required-key :time-end         ) s/Str
     (s/required-key :time-start       ) s/Str
     (s/required-key :took             ) s/Num

     (s/optional-key :architecture     ) s/Str
     (s/optional-key :args             ) [s/Str]
     (s/optional-key :branch           ) s/Str
     (s/optional-key :cwd              ) s/Str
     (s/optional-key :end-millis       ) s/Num
     (s/optional-key :hardwaremodel    ) s/Str
     (s/optional-key :hostname         ) s/Str
     (s/optional-key :ipaddress        ) s/Str
     (s/optional-key :ipaddress6       ) s/Str
     (s/optional-key :jenkins-executor ) (s/maybe s/Str)
     (s/optional-key :jenkins-labels   ) (s/maybe s/Str)
     (s/optional-key :jenkins-node     ) (s/maybe s/Str)
     (s/optional-key :jenkins-number   ) (s/maybe s/Str)
     (s/optional-key :job-name-extra   ) s/Str
     (s/optional-key :kernelrelease    ) s/Str
     (s/optional-key :kernelversion    ) s/Str
     (s/optional-key :memorysize_mb    ) s/Str
     (s/optional-key :operatingsystem  ) s/Str
     (s/optional-key :operatingsystemrelease ) s/Str
     (s/optional-key :physicalprocessorcount ) s/Num
     (s/optional-key :processor0       ) s/Str
     (s/optional-key :processorcount   ) (s/cond-pre s/Str s/Num)
     (s/optional-key :profile-name     ) s/Str
     (s/optional-key :program          ) s/Str
     (s/optional-key :timezone         ) s/Str
     (s/optional-key :uptime_days      ) s/Num
     (s/optional-key :url              ) (s/maybe s/Str)
     (s/optional-key :workspace        ) s/Str
     })
