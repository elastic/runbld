(ns runbld.java
  (:require [runbld.schema :refer :all]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+]])
  (:require [clojure.edn]
            [clojure.string :as str]
            [runbld.util.io :as io]))

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
                   m (map keyword (str/split k (re-pattern "\\."))) v))]
          (clojure.pprint/pprint
           (reduce
            f {} (select-keys
                  (into (sorted-map) (System/getProperties)) ks))))))))

(defn no-jre-path [path]
  (when path
    (if (.endsWith path "/jre")
      (str/replace path #"^(.*)/jre$" "$1")
      path)))

(defn java-home-java [{:keys [java-home]}]
  (when java-home
    (str (io/file (no-jre-path java-home) "bin/java"))))

(defn jvm-facts [opts]
  (io/with-tmp-source [clj (source)]
    (let [classpath (System/getProperty "java.class.path")
          java-bin (or (java-home-java opts)
                       (io/which "java")
                       (throw+ {:error ::no-java
                                :msg "can't find a java binary"}))
          cmd [java-bin "-cp" classpath "clojure.main" (str clj)]]
      (clojure.edn/read-string
       (:out (apply io/run cmd))))))

(s/defn wrap-java :- OptsWithJava
  [proc]
  (fn [opts]
    (proc (merge opts (jvm-facts opts)))))
