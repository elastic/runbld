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

(defn remove-jre [path]
  (when path
    (if (or (.endsWith path "/jre")
            (.contains path "/jre/"))
      (str/replace path #"^(.*)/jre(/.+)?$" "$1$2")
      path)))

(defn java-home
  ([]
   (or (java-home "javac") ;; for JDK
       (java-home "java")))
  ([java-bin]
   (-> (io/resolve-binary java-bin)
       io/file
       .getParent
       io/file
       .getParent)))

(defn java-home-java
  ([]
   (java-home-java (java-home)))
  ([path]
   (when path
     (str (io/file path "bin/java")))))

(defn jvm-facts [opts]
  (io/with-tmp-source [clj (source)]
    (let [classpath (System/getProperty "java.class.path")
          java-home (-> opts :process :env :JAVA_HOME)
          java-bin (java-home-java java-home)
          cmd [java-bin "-cp" classpath "clojure.main" (str clj)]]
      (when (io/resolve-binary java-bin)
        (let [facts (clojure.edn/read-string
                     (:out (apply io/run cmd)))]
          (assoc-in facts [:java :home] java-home))))))

(s/defn wrap-java
  [proc]
  (fn [opts]
    (proc (merge opts (jvm-facts opts)))))
