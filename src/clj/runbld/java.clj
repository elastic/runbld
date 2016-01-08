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

(defn java-home
  ([]
   (java-home "java"))
  ([java-bin]
   (-> (io/resolve-binary java-bin)
       io/file
       .getParent
       io/file
       .getParent
       no-jre-path)))

(defn java-home-java [java-home allow-jre]
  (when-let [jh (if allow-jre java-home (no-jre-path java-home))]
    (str (io/file jh "bin/java"))))

(defn find-java [opts]
  (or (java-home-java (:java-home opts) (-> opts :java :allow-jre))
      (io/which "java")
      (throw+ {:error ::no-java
               :msg "can't find a java binary"})))

(defn jvm-facts [opts]
  (io/with-tmp-source [clj (source)]
    (let [classpath (System/getProperty "java.class.path")
          java-bin (find-java opts)
          cmd [java-bin "-cp" classpath "clojure.main" (str clj)]
          facts (clojure.edn/read-string
                 (:out (apply io/run cmd)))]
      ;; have to check jre again because of what actually gets
      ;; returned in the java.home property
      (if (-> opts :java :allow-jre)
        facts
        (update-in facts [:java :home] no-jre-path)))))

(s/defn wrap-java
  [proc]
  (fn [opts]
    (proc (merge opts (jvm-facts opts)))))
