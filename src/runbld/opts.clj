(ns runbld.opts
  (:import (org.yaml.snakeyaml Yaml))
  (:require [clojure.java.io :as io]
            [clojure.tools.cli :as cli]
            [elasticsearch.connection.http :as es]
            [runbld.util.data :refer [deep-merge-with deep-merge
                                      keywordize-keys]]
            [runbld.version :as version]
            [slingshot.slingshot :refer [throw+]]))

(def ^:dynamic *dev* true)

(def defaults
  {:es.url "http://localhost:9200"
   :es.index.build "'build'-yyyy-MM"
   :es.index.config "runbld"})

(defn load-config [filepath]
  (let [f (io/file filepath)]
    (when (not (.isFile f))
      (throw+ {:error ::file-not-found
               :msg (format "config file %s not found"
                            filepath)}))
    (->> (.load (Yaml.) (slurp f))
         (into {})
         keywordize-keys)))

(defn system-config []
  (io/file
   (if (.startsWith (System/getProperty "os.name") "Windows")
     "c:\\runbld.conf"
     "/etc/runbld.conf")))

(defn merge-opts-with-file [opts]
  (deep-merge-with deep-merge
                   defaults
                   (if *dev*
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
               :msg (version/version)}))

    (when (not (= 1 (count arguments)))
      (throw+ {:help ::usage
               :msg (format "runbld %s\nusage: rundmc /path/to/script.bash"
                            (version/version))}))

    {:opts (assoc options
                  :scriptfile (first arguments))
     :es.conn (es/make (merge {:url (:es.url options)}
                              (:es.http-opts options)))}))
