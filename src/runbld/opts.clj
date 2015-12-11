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
            [slingshot.slingshot :refer [throw+]]))

(def defaults
  {:es
   {:url "http://localhost:9200"
    :index "'build'-yyyy-MM"
    :http-opts {:insecure? false}}

   :s3
   {:bucket "test.example.com"
    :prefix "/"
    :access-key "key"
    :secret-key "secret"}

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

(s/defn merge-profiles :- java.util.Map
  [job-name :- s/Str
   profiles :- [java.util.Map]]
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
   filepath :- s/Str]
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
  [{:keys [job-name] :as opts}]
  (deep-merge-with deep-merge
                   defaults
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
  {:process (select-keys cli-opts [:program :args :cwd])
   :job-name (:job-name cli-opts)
   :configfile (:config cli-opts)
   :version (:version cli-opts)})

(def opts
  [["-v" "--version" "Print version"]
   ["-c" "--config FILE" "Config file"]
   ["-d" "--cwd DIR" "Set CWD for the process"
    :default (System/getProperty "user.dir")]
   ["-j" "--job-name JOBNAME" (str "Job name: org,project,branch,etc "
                                   "also read from $JOB_NAME")
    :default (environ/env :job-name)]
   ["-p" "--program PROGRAM" "Program that will run the scriptfile"
    :default "bash"]
   ["-a" "--args ARGS" "Args to pass PROGRAM"
    :default ["-x"]]])

(s/defn parse-args :- Opts
  [args :- [s/Str]]
  (let [{:keys [options arguments summary errors]
         :as parsed-opts} (cli/parse-opts args opts)
        options (assemble-all-opts
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
           {:es (let [idx (expand-date-pattern
                           (-> options :es :index))
                      es-opts (assoc (:es options) :index idx)]
                  {:index idx
                   :conn (es/make es-opts)})

            :process (merge
                      ;; Invariant: Jenkins passes it in through arguments
                      {:scriptfile (first arguments)}
                      (:process options))})))
