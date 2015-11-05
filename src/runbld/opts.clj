(ns runbld.opts
  (:require [clj-yaml.core :as yaml]
            [clojure.java.io :as io]
            [clojure.tools.cli :as cli]
            [elasticsearch.connection.http :as es]
            [environ.core :as environ]
            [runbld.util.data :refer [deep-merge-with deep-merge]]
            [runbld.util.date :as date]
            [runbld.version :as version]
            [slingshot.slingshot :refer [throw+]]))

(def defaults
  {:es.url "http://localhost:9200"
   :es.index.build "'build'-yyyy-MM"
   :es.index.config "runbld"

   :email
   {:host "localhost"
    :port 587
    :tls true}})

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
                   (if (:config opts)
                     (load-config (:config opts))
                     {})
                   opts))

(def opts
  [["-v" "--version" "Print version"]
   ["-c" "--config FILE" "Config file"]
   [nil "--default-job-name JOB_NAME" "Job name: org,project,branch"]
   [nil "--es.url URL" "Elasticsearch endpoint"]
   [nil "--es.index.build INDEX" "ES index for build results"]
   [nil "--es.index.config INDEX" "ES index for configuration"]])

(defn parse-args [args]
  (let [{:keys [options arguments summary errors]
         :as parsed-opts} (cli/parse-opts args opts)
        options (merge-opts-with-file options)]
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
               :msg (format "runbld %s\nusage: rundmc /path/to/script.bash"
                            (version/string))}))

    {:errors (atom [])
     :opts (assoc options
                  ;; Invariant: Jenkins passes it in through arguments
                  :scriptfile (first arguments)
                  :es.index.build (expand-date-pattern
                                   (options :es.index.build)))
     :es.conn (es/make (merge {:url (:es.url options)}
                              (:es.http-opts options)))}))
