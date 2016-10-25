(ns runbld.store.mapping
  (:refer-clojure :exclude [boolean long double]))

(def doc-type :t)

(def not-analyzed
  {:type :string
   :index :not_analyzed})

(def analyzed
  {:type :string})

(def long
  {:type :long})

(def double
  {:type :double})

(def boolean
  {:type :boolean})

(def date
  {:type :date})

(def multi-string
  (merge
   not-analyzed
   {:fields {:analyzed
             {:type :string
              :index :analyzed}}}))

(def classpath-analyzer
  {:classpath
   {:type :pattern
    :pattern ":|;"}})

(def classpath
  {:type :string
   :analyzer "classpath"})

(def stored-log-index-settings
  {:mappings
   {doc-type
    {:_all {:enabled false}
     :properties
     {:build-id not-analyzed
      :stream   not-analyzed
      :time     date
      :log      analyzed
      :size     long
      :ord
      {:properties
       {:total  long
        :stream long}}}}}})

(def stored-build-index-settings
  {:settings
   {:analysis
    {:analyzer classpath-analyzer}}
   :mappings
   {doc-type
    {:_all {:enabled false}
     :properties
     {:id not-analyzed
      :version {:properties
                {:string not-analyzed
                 :hash   not-analyzed}}
      :build {:properties
              {:org                 not-analyzed
               :project             not-analyzed
               :branch              not-analyzed
               :job-name-extra      multi-string
               :job-name            multi-string
               :org-project-branch  not-analyzed
               :scheduler           not-analyzed
               :url                 multi-string
               :console-url         multi-string
               :tags                not-analyzed
               :number              not-analyzed
               :executor            not-analyzed
               :node                not-analyzed}}
      :sys {:properties
            {:arch            not-analyzed
             :cpu-type        multi-string
             :cpus            long
             :cpus-physical   long
             :datacenter      not-analyzed
             :provider        not-analyzed
             :hostname        not-analyzed
             :facter-provider not-analyzed
             :facter-version  not-analyzed
             :fs-mountpoint   not-analyzed
             :fs-type         not-analyzed
             :fs-bytes-total  long
             :fs-bytes-free   long
             :fs-bytes-used   long
             :fs-percent-free double
             :fs-percent-used double
             :image-id        not-analyzed
             :instance-id     not-analyzed
             :instance-type   not-analyzed
             :ip4             not-analyzed
             :ip6             not-analyzed
             :kernel-name     not-analyzed
             :kernel-release  not-analyzed
             :kernel-version  not-analyzed
             :model           not-analyzed
             :os              not-analyzed
             :os-version      not-analyzed
             :ram-mb          double
             :ram-gb          double
             :region          not-analyzed
             :timezone        not-analyzed
             :uptime          analyzed
             :uptime-days     long
             :uptime-secs     long
             :virtual         boolean}}
      :vcs {:properties
            {:author-name   multi-string
             :commit-id     not-analyzed
             :commit-short  not-analyzed
             :commit-time   date
             :message       analyzed
             :provider      not-analyzed
             :log-pretty    analyzed
             :project-url   not-analyzed

             :branch-url   not-analyzed
             :commit-url   not-analyzed
             :author-email multi-string
             :author-time  date
             :commit-email not-analyzed
             :commit-name  multi-string
             :message-full analyzed}}
      :process {:properties
                {:cmd             not-analyzed
                 :cmd-source      multi-string
                 :err-accuracy    long
                 :err-bytes       long
                 :err-file-bytes  long
                 :exit-code       long
                 :millis-end      long
                 :millis-start    long
                 :out-accuracy    long
                 :out-bytes       long
                 :out-file-bytes  long
                 :status          not-analyzed
                 :time-end        date
                 :time-start      date
                 :took            long}}
      :test {:properties
             {:errors   long
              :failures long
              :tests    long
              :skipped  long
              :failed-testcases
              {:properties
               {:error-type not-analyzed
                :class      not-analyzed
                :test       not-analyzed
                :type       not-analyzed
                :summary    analyzed
                :message    multi-string}}}}
      :java {:properties
             {:home      multi-string
              :vendor    multi-string
              :version   multi-string
              :class
              {:properties
               {:path    classpath}}
              :runtime
              {:properties
               {:name    multi-string
                :version multi-string}}
              :vm
              {:properties
               {:info    multi-string
                :name    multi-string
                :vendor  multi-string
                :version multi-string}}}}}}}})

(def stored-failure-index-settings
  {:mappings
   {doc-type
    {:_all {:enabled false}
     :properties
     {:build-id not-analyzed}}}})

