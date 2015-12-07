(ns runbld.opts
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [clj-yaml.core :as yaml]
            [clojure.java.io :as io]
            [clojure.tools.cli :as cli]
            [elasticsearch.connection.http :as es]
            [environ.core :as environ]
            [runbld.util.data :refer [deep-merge-with deep-merge]]
            [runbld.util.date :as date]
            [runbld.version :as version]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+]])
  (:import (runbld.schema OptsProcess)))

(def Opts
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
   (s/required-key :process) OptsProcess
   (s/required-key :build) {s/Keyword s/Any}
   (s/required-key :report) {s/Keyword s/Any}
   (s/optional-key :facter) {s/Keyword s/Any}
   (s/optional-key :profiles) {s/Keyword s/Any}
   (s/optional-key :version) (s/maybe s/Bool)
   (s/optional-key :config-file) (s/maybe s/Str)
   })

(def defaults
  {:build
   {:job-name (System/getenv "JOB_NAME")}

   :es
   {:url "http://localhost:9200"
    :index "'build'-yyyy-MM"
    :http-opts {:insecure? false}}

   :process
   {:program "bash"
    :args ["-x"]
    :cwd (System/getProperty "user.dir")}

   :email
   {:host "localhost"
    :port 587
    :tls true
    :template-txt "templates/email.mustache.txt"
    :template-html "templates/email.mustache.html"
    :text-only false
    :max-failure-notify 10}})

(defn expand-date-pattern [s]
  (if (string? s)
    (if (.contains s "'")
      (date/expand s)
      s)
    (throw+ {:error ::invalid-date
             :msg "date pattern should be a string"
             :arg s})))

(defn load-config [filepath]
  (let [f (io/file filepath)]
    (when (not (.isFile f))
      (throw+ {:error ::file-not-found
               :msg (format "config file %s not found"
                            filepath)}))
    (yaml/parse-string (slurp f))))

(defn system-config []
  (io/file
   (if (.startsWith (System/getProperty "os.name") "Windows")
     "c:\\runbld\\runbld.conf"
     "/etc/runbld/runbld.conf")))

(defn merge-opts-with-file [opts]
  (deep-merge-with deep-merge
                   defaults
                   (if (environ/env :dev)
                     {}
                     (let [sys (system-config)]
                       (if (.isFile sys)
                         (load-config (system-config))
                         {})))
                   (if (:config-file opts)
                     (load-config (:config-file opts))
                     {})
                   opts))

(defn normalize
  "Normalize the tools.cli map to the local structure."
  [cli-opts]
  {:process (select-keys cli-opts [:program :args])
   :build (select-keys cli-opts [:job-name])
   :config-file (:config cli-opts)
   :version (:version cli-opts)})

(def opts
  [["-v" "--version" "Print version"]
   ["-c" "--config FILE" "Config file"]
   [nil "--job-name JOBNAME" (str "Job name: org,project,branch, "
                                  "also read from $JOB_NAME")]
   [nil "--program PROGRAM" "Program that will run the scriptfile"]
   [nil "--args ARGS" "Args to pass PROGRAM"]])

(defn parse-args [args]
  (let [{:keys [options arguments summary errors]
         :as parsed-opts} (cli/parse-opts args opts :no-defaults true)
        options (merge-opts-with-file
                 (normalize options))]
    (when (pos? (count errors))
      (throw+ {:error ::parse-error
               :msg (with-out-str
                      (doseq [err errors]
                        (println err)))}))

    (when (:version options)
      (throw+ {:help ::version
               :msg (version/string)}))

    (when (not (= 1 (count arguments)))
      (throw+ {:help ::usage
               :msg (format "runbld %s\nusage: runbld /path/to/script.bash"
                            (version/string))}))

    (merge options
           {:errors (atom [])
            :es {:index (expand-date-pattern (-> options :es :index))
                 :conn (es/make (:es options))}

            :process (merge
                      ;; Invariant: Jenkins passes it in through arguments
                      {:scriptfile (first arguments)}
                      (:process options))})))
