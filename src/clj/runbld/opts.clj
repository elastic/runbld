(ns runbld.opts
  (:require [runbld.schema :refer :all]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+]])
  (:require [clj-yaml.core :as yaml]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [environ.core :as environ]
            [runbld.store :as store]
            [runbld.util.data :refer [deep-merge-with deep-merge]]
            [runbld.util.date :as date]
            [runbld.io :as io]
            [runbld.version :as version]))

(def config-file-defaults
  {:es
   {:url "http://localhost:9200"
    :build-index   "build"
    :failure-index "failure"
    :log-index     "log"
    :http-opts {:insecure? false}
    :max-index-bytes store/MAX_INDEX_BYTES
    :bulk-timeout-ms 2000
    :bulk-size 500}

   :s3
   {:bucket "test.example.com"
    :prefix "/"
    :access-key "key"
    :secret-key "secret"}

   :java
   {;; java.home property always shows ".../jre", this forces JDK
    :allow-jre false}

   :process
   {:inherit-exit-code true
    :cwd (System/getProperty "user.dir")
    :stdout ".stdout.log"
    :stderr ".stderr.log"
    :output ".output.log"}

   :email
   {:host "localhost"
    :port 587
    :tls true
    :template-txt "templates/email.mustache.txt"
    :template-html "templates/email.mustache.html"
    :text-only false
    :max-failure-notify 10}})

(s/defn merge-profiles :- java.util.Map
  [job-name :- s/Str
   profiles :- [{s/Keyword s/Any}]]
  (if profiles
    (apply deep-merge-with deep-merge
           (for [ms profiles]
             (let [[k v] (first ms)
                   pat ((comp re-pattern name) k)]
               (if (re-find pat job-name)
                 v
                 {}))))
    {}))

(defn load-config [filepath]
  (let [f (io/file filepath)]
    (when (not (.isFile f))
      (throw+ {:error ::file-not-found
               :msg (format "config file %s not found"
                            filepath)}))
    (yaml/parse-string (slurp f))))

(s/defn load-config-with-profiles :- java.util.Map
  [job-name :- s/Str
   filepath :- (s/cond-pre s/Str java.io.File)]
  (let [conf (load-config filepath)
        res (deep-merge-with deep-merge
                             (dissoc conf :profiles)
                             (merge-profiles job-name (:profiles conf)))]
    res))

(defn system-config []
  (io/file
   (if (.startsWith (System/getProperty "os.name") "Windows")
     "c:\\runbld\\runbld.conf"
     "/etc/runbld/runbld.conf")))

(s/defn assemble-all-opts :- java.util.Map
  [{:keys [job-name] :as opts} :- {(s/required-key :job-name) s/Str
                                   s/Keyword s/Any}]
  (deep-merge-with deep-merge
                   config-file-defaults
                   (if (environ/env :dev)
                     {}
                     (let [sys (system-config)]
                       (if (.isFile sys)
                         (load-config-with-profiles job-name (system-config))
                         {})))
                   (if (:configfile opts)
                     (load-config-with-profiles job-name (:configfile opts))
                     {})
                   opts))

(defn normalize
  "Normalize the tools.cli map to the local structure."
  [cli-opts]
  (merge
   {:process (select-keys cli-opts [:program :args :cwd])
    :job-name (:job-name cli-opts)
    :configfile (:config cli-opts)
    :version (:version cli-opts)}
   (when (:java-home cli-opts)
     {:java-home (:java-home cli-opts)})))

(def opts
  [["-v" "--version" "Print version"]
   ["-c" "--config FILE" "Config file"]
   ["-d" "--cwd DIR" "Set CWD for the process"]
   ["-j" "--job-name JOBNAME" (str "Job name: org,project,branch,etc "
                                   "also read from $JOB_NAME")
    :default (environ/env :job-name)]
   [nil "--java-home PATH" "If different from JAVA_HOME"
    :default (System/getenv "JAVA_HOME")]
   ["-p" "--program PROGRAM" "Program that will run the scriptfile"
    :default "bash"]
   ["-a" "--args ARGS" "Args to pass PROGRAM"
    :default ["-x"]
    :parse-fn #(str/split % #" ")]
   ["-h" "--help" "Help me"]])

(s/defn set-up-es [{:keys [url
                           build-index
                           failure-index
                           log-index
                           max-index-bytes] :as opts}]
  (let [conn (store/make-connection
              (select-keys opts [:url :http-opts]))
        build-index-write (store/set-up-index
                           conn build-index
                           StoredBuildIndexSettings
                           max-index-bytes)
        failure-index-write (store/set-up-index
                             conn failure-index
                             StoredFailureIndexSettings
                             max-index-bytes)
        log-index-write (store/set-up-index
                         conn log-index
                         StoredLogIndexSettings
                         max-index-bytes)]
    (-> opts
        (assoc :build-index-search (format "%s*" build-index))
        (assoc :failure-index-search (format "%s*" failure-index))
        (assoc :log-index-search (format "%s*" log-index))
        (assoc :build-index-write build-index-write)
        (assoc :failure-index-write failure-index-write)
        (assoc :log-index-write log-index-write)
        (assoc :conn conn))))

(s/defn parse-args :- Opts
  ([args :- [s/Str]]
   (let [{:keys [options arguments summary errors]
          :as parsed-opts} (cli/parse-opts args opts :nodefault true)]

     (when (:help options)
       (throw+ {:help ::usage
                :msg summary}))

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

     (when (not (:job-name options))
       (throw+ {:help ::usage
                :msg "must set -j or $JOB_NAME"}))

     (let [options (assemble-all-opts
                    (normalize options))]
       (merge options
              {:es (set-up-es (:es options))
               :process (-> (:process options)
                            ;; Invariant: Jenkins passes it in through arguments
                            (assoc :scriptfile (first arguments))
                            ;; Go ahead and resolve
                            (update :cwd (comp str io/abspath-file)))
               :version {:string (version/version)
                         :hash (version/build)}})))))
