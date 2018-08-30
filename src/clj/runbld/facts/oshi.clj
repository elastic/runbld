(ns runbld.facts.oshi
  (:require
   [cheshire.core :as json]
   [clojure.java.shell :as sh]
   [clojure.string :as string]
   [clojure.walk :as walk]
   [runbld.facts :refer [Facter] :as facts]
   [runbld.io :as rio]
   [runbld.util.data :as data]
   [runbld.util.debug :as debug])
  (:import
   (oshi.json SystemInfo)))

;; just wanted the defrecord near the top
(declare facts primary-network)

(defrecord Oshi [facts]
  Facter

  (raw [x] x)

  (arch [{x :facts}]
    (get-in x [:properties :os.arch]))

  (model [{x :facts}]
    ;; The field that we use from facter is [:os :hardware] which
    ;; appears to basically return the system architecture.
    (let [arch (get-in x [:properties :os.arch])]
      (if (= "amd64" arch)
        "x86_64"
        arch)))

  (cpu-type [{x :facts}]
    (get-in x [:hardware :processor :name]))

  (cpus [{x :facts}]
    (get-in x [:hardware :processor :logicalProcessorCount]))

  (cpus-physical [{x :facts}]
    (get-in x [:hardware :processor :physicalProcessorCount]))

  (facter-provider [{x :facts}]
    "oshi")

  (facter-version [{x :facts}]
    (->> (get-in x [:properties :java.class.path])
         (re-find #".*oshi-json/((?:\d+\.?)+)/oshi-json")
         last))

  (hostname [{x :facts}]
    (get-in x [:operatingSystem :networkParams :hostName]))

  (ip4 [{x :facts}]
    (seq (mapcat :ipv4 (get-in facts [:hardware :networks]))))

  (ip6 [{x :facts}]
    (seq (mapcat :ipv6 (get-in facts [:hardware :networks]))))

  (kernel-name [{x :facts}]
    (get-in x [:uname :name]))

  (kernel-release [{x :facts}]
    (get-in x [:uname :version]))

  (kernel-version [{x :facts}]
    (first
     (string/split
      (get-in x [:uname :version]) #"-")))

  (os [{x :facts}]
    (get-in x [:operatingSystem :family]))

  (os-version [{x :facts}]
    (get-in x [:operatingSystem :version :version]))

  (os-family [{x :facts}]
    (get-in x [:operatingSystem :family]))

  (ram-mb [{x :facts}]
    (data/bigdec
     (/ (get-in x [:hardware :memory :total])
        (* 1024 1024))
     2))

  (ram-gb [{x :facts}]
    (data/bigdec
     (/ (get-in x [:hardware :memory :total])
        (* 1024 1024 1024))
     2))

  (ram-bytes [{x :facts}]
    (get-in x [:hardware :memory :total]))

  (timezone [{x :facts}]
    (get-in x [:properties :user.timezone]))

  (uptime-days [{x :facts}]
    (int
     (/ (get-in x [:hardware :processor :systemUptime])
        (* 60 60 24))))

  (uptime-secs [{x :facts}]
    (get-in x [:hardware :processor :systemUptime]))

  (uptime [{x :facts :as raw}]
    (let [days (.uptime-days raw)]
      (str days
           (if (> days 1)
             " days"
             " day")))))

(defn primary-network
  "Takes the set of facts and returns a vector of [ipv4 ipv6] for the
  primary network adapter.  Is somewhat naive in that it picks the
  primary adapter based on total bytes sent and received."
  [facts]
  ;; TODO - is this sufficient to find the "primary"?
  (->> (get-in facts [:hardware :networks])
       (remove (comp empty? :ipv4))
       (sort-by #(+ (:bytesSent %)
                    (:bytesRecv %)))
       last
       ((juxt :ipv4 :ipv6))
       (map first)))

(defn uname [{:keys [platform] :as facts}]
  (let [platform (string/lower-case platform)]
    (cond
      (#{"linux" "macosx"} platform)
      {:name (string/trim (:out (rio/run "uname" "-s")))
       :version (string/trim (:out (rio/run "uname" "-r")))}

      (= "windows" platform)
      (let [sysinfo (:out (rio/run "cmd.exe" "-c" "systeminfo"))]
        {:name "Windows"
         :version (last (re-find #"(\d+\.?)+" sysinfo))}))))

(defn facts* []
  (let [start (System/currentTimeMillis)
        system-facts (json/parse-string (str (SystemInfo.)) true)
        oshi-time (System/currentTimeMillis)
        system-props (walk/keywordize-keys (into {} (System/getProperties)))
        props-time (System/currentTimeMillis)
        uname-props (uname system-facts)
        uname-time (System/currentTimeMillis)]
    (debug/log "Collected system facts."
               "OSHI took:" (- oshi-time start) "ms."
               "Reading sys props took:" (- props-time oshi-time) "ms."
               "Reading uname took:" (- uname-time props-time) "ms.")
    (assoc system-facts
           :properties system-props
           :uname (uname system-facts))))

(def facts (memoize facts*))
