(ns runbld.java
  (:require [runbld.schema :refer :all]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+]])
  (:require [clojure.edn]
            [clojure.string :as str]
            [runbld.io :as io]))

(defn source []
  (with-out-str
    (prn
     '(do
        (require 'clojure.pprint)
        (require '[clojure.string :as str])
        (let [ks ["java.class.path"
                  "java.home"
                  "java.runtime.name"
                  "java.runtime.version"
                  "java.vendor"
                  "java.version"
                  "java.vm.info"
                  "java.vm.name"
                  "java.vm.vendor"
                  "java.vm.version"]
              f (fn [m [k v]]
                  (assoc-in
                   m (->> (str/split k (re-pattern "\\."))
                          (map keyword)
                          (drop 1)) v))]
          (clojure.pprint/pprint
           (reduce
            f {} (select-keys
                  (into (sorted-map) (System/getProperties)) ks))))))))

(defn remove-jre [path]
  (when path
    (if (or (.endsWith path "/jre")
            (.endsWith path "\\jre")
            (.contains path "/jre/")
            (.contains path "\\jre\\"))
      (str (io/file path ".."))
      path)))

(def discover-properties
  (memoize
   (fn [java-bin]
     (io/with-tmp-source [clj (source)]
       (let [classpath (System/getProperty "java.class.path")
             cmd [java-bin "-cp" classpath "clojure.main" (str clj)]]
         (when (io/resolve-binary java-bin)
           (clojure.edn/read-string
            (:out (apply io/run cmd)))))))))

(defn jvm-facts
  ([]
   (jvm-facts nil))
  ([bootstrap-java-home]
   (let [java-bin (if bootstrap-java-home
                    ;; specified by user
                    (str (io/file bootstrap-java-home "bin" "java"))
                    ;; use JVM on PATH
                    (io/resolve-binary "java"))
         props (discover-properties java-bin)
         adjusted-java-home (io/abspath
                             (remove-jre
                              (:home props)))]
     (assoc props :home adjusted-java-home))))

(s/defn wrap-java
  [proc]
  (fn [opts]
    (if (get-in opts [:java :home])
      (proc opts)
      (let [facts (jvm-facts (:bootstrap-java-home opts))]
        (proc
         (-> opts
             (merge facts)
             (assoc-in [:process :env :JAVA_HOME] (:home facts))))))))
