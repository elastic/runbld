(ns runbld.schema
  (:require [schema.core :as s]
            [runbld.scheduler :refer [Scheduler]]
            [runbld.schema.mapping :as m])
  (:import (elasticsearch.connection Connection)))

(def DocType :t)

(def VersionInfo
  {:string s/Str
   :hash   s/Str})

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
   (s/required-key :scriptfile) s/Str
   (s/required-key :inherit-exit-code ) s/Bool})

(def OptsElasticsearch
  {(s/required-key :build-index          ) s/Str
   (s/required-key :build-index-search   ) s/Str
   (s/required-key :build-index-write    ) s/Str
   (s/required-key :failure-index        ) s/Str
   (s/required-key :failure-index-search ) s/Str
   (s/required-key :failure-index-write  ) s/Str
   (s/required-key :max-index-bytes      ) s/Num
   (s/required-key :conn                 ) Connection
   (s/optional-key :http-opts            ) {s/Keyword s/Any}
   (s/optional-key :url                  ) s/Str})

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
  {:arch                   s/Str
   :cpu-type               s/Str
   :cpus                   s/Num
   :cpus-physical          s/Num
   :hostname               s/Str
   :ipv4                   s/Str
   :ipv6                   s/Str
   :kernel-name            s/Str
   :kernel-release         s/Str
   :kernel-version         s/Str
   :model                  s/Str
   :os                     s/Str
   :os-version             s/Str
   :ram-mb                 s/Num
   :ram-gb                 s/Num
   :timezone               s/Str
   :uptime                 s/Str
   :uptime-days            s/Num
   :uptime-secs            s/Num
   :virtual                s/Bool})

(def Env
  {s/Str s/Any})

(def Build
  {:org                 s/Str
   :project             s/Str
   :branch              s/Str
   :job-name-extra      s/Str
   :job-name            s/Str
   :org-project-branch  s/Str
   :scheduler-type      s/Str
   :url                 s/Str
   :console-url         s/Str
   :tags                [s/Str]
   (s/optional-key :number) s/Str
   (s/optional-key :executor) s/Str
   (s/optional-key :node) s/Str
   })

(def SchedulerInfo
  {(s/required-key :type     ) (s/maybe s/Str)
   (s/required-key :url      ) (s/maybe s/Str)
   (s/required-key :node     ) (s/maybe s/Str)})

(def OptsStage2
  (merge Opts {(s/required-key :sys) BuildSystem
               (s/required-key :logger) clojure.lang.IFn}))

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

(def StoredProcessResult
  {:cmd             [s/Str]
   :cmd-source      s/Str
   :err-accuracy    s/Num
   :err-bytes       s/Num
   :err-file-bytes  s/Num
   :exit-code       s/Num
   :millis-end      s/Num
   :millis-start    s/Num
   :out-accuracy    s/Num
   :out-bytes       s/Num
   :out-file-bytes  s/Num
   :status          s/Str
   :time-end        s/Str
   :time-start      s/Str
   :took            s/Num})

(def VcsLog
  {
   :author-name   s/Str
   :commit-id     s/Str
   :commit-short  s/Str
   :commit-time   s/Str
   :message       s/Str
   :type          s/Str
   :log-pretty    s/Str
   :project-url   s/Str

   (s/optional-key :branch-url   ) s/Str
   (s/optional-key :commit-url   ) s/Str
   (s/optional-key :author-email ) s/Str
   (s/optional-key :author-time  ) s/Str
   (s/optional-key :commit-email ) s/Str
   (s/optional-key :commit-name  ) s/Str
   (s/optional-key :message-full ) s/Str
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

(def StoredTestSummary
  {:errors   s/Num
   :failures s/Num
   :tests    s/Num
   :skipped  s/Num
   :failed-testcases
   [{:error-type s/Str
     :class      s/Str
     :test       s/Str
     :type       s/Str
     :summary    s/Str
     (s/optional-key :message) s/Str}]})

(def StoredFailure
  (merge
   FailedTestCase
   {(s/required-key :build-id ) s/Str
    (s/required-key :time     ) s/Str
    (s/required-key :org      ) s/Str
    (s/required-key :project  ) s/Str
    (s/required-key :branch   ) s/Str}))

(def StoredBuild
  {:id s/Str
   :version VersionInfo
   :build Build
   :sys BuildSystem
   :vcs VcsLog
   :process StoredProcessResult
   :test (s/maybe StoredTestSummary)})

(def StoredBuildMapping
  {DocType
   {:properties
    {:id m/not-analyzed
     :version {:properties
               {:string m/not-analyzed
                :hash   m/not-analyzed}}
     :build {:properties
             {:org                 m/not-analyzed
              :project             m/not-analyzed
              :branch              m/not-analyzed
              :job-name-extra      m/multi-string
              :job-name            m/multi-string
              :org-project-branch  m/not-analyzed
              :scheduler-type      m/not-analyzed
              :url                 m/multi-string
              :console-url         m/multi-string
              :tags                m/not-analyzed
              :number              m/not-analyzed
              :executor            m/not-analyzed
              :node                m/not-analyzed}}
     :sys {:properties
           {:arch           m/not-analyzed
            :cpu-type       m/not-analyzed
            :cpus           m/long
            :cpus-physical  m/long
            :hostname       m/not-analyzed
            :ipv4           m/not-analyzed
            :ipv6           m/not-analyzed
            :kernel-name    m/not-analyzed
            :kernel-release m/not-analyzed
            :kernel-version m/not-analyzed
            :model          m/not-analyzed
            :os             m/not-analyzed
            :os-version     m/not-analyzed
            :ram-mb         m/double
            :ram-gb         m/double
            :timezone       m/not-analyzed
            :uptime         m/analyzed
            :uptime-days    m/long
            :uptime-secs    m/long
            :virtual        m/boolean}}
     :vcs {:properties
           {:author-name   m/multi-string
            :commit-id     m/not-analyzed
            :commit-short  m/not-analyzed
            :commit-time   m/date
            :message       m/analyzed
            :type          m/not-analyzed
            :log-pretty    m/analyzed
            :project-url   m/not-analyzed

            :branch-url   m/not-analyzed
            :commit-url   m/not-analyzed
            :author-email m/multi-string
            :author-time  m/date
            :commit-email m/not-analyzed
            :commit-name  m/multi-string
            :message-full m/analyzed}}
     :process {:properties
               {:cmd             m/not-analyzed
                :cmd-source      m/multi-string
                :err-accuracy    m/long
                :err-bytes       m/long
                :err-file-bytes  m/long
                :exit-code       m/long
                :millis-end      m/long
                :millis-start    m/long
                :out-accuracy    m/long
                :out-bytes       m/long
                :out-file-bytes  m/long
                :status          m/not-analyzed
                :time-end        m/date
                :time-start      m/date
                :took            m/long}}
     :test {:properties
            {:errors   m/long
             :failures m/long
             :tests    m/long
             :skipped  m/long
             :failed-testcases
             {:properties
              {:error-type m/not-analyzed
               :class      m/not-analyzed
               :test       m/not-analyzed
               :type       m/not-analyzed
               :summary    m/analyzed
               :message    m/multi-string}}}}
     }}})

(def StoredFailureMapping
  {DocType
   {:properties
    {:build-id m/not-analyzed}}})

(def EmailCtx
  {(s/required-key :id     ) s/Str
   (s/required-key :version) VersionInfo
   (s/required-key :build  ) Build
   (s/required-key :sys    ) BuildSystem
   (s/required-key :email  ) {(s/required-key :to) s/Str
                              (s/required-key :subject) s/Str
                              }
   (s/required-key :vcs    ) VcsLog
   (s/required-key :process) (merge StoredProcessResult
                                    {(s/required-key :cmd) s/Str
                                     (s/required-key :args) s/Str
                                     (s/required-key :took-human) s/Str})
   (s/required-key :test) (s/maybe StoredTestSummary)})
