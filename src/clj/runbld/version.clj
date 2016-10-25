(ns runbld.version
  (:require [clojure.spec :as s]
            [clojure.java.io :as io]))

(s/def ::string string?)
(s/def ::hash string?)
(s/def ::version (s/keys :req-un [::string ::hash]))

(defn build []
  (if-let [b (io/resource "build.txt")]
    (.trim (slurp b))
    "NOBUILD" #_"For fresh tests"))

(defn version []
  (.trim (slurp (io/resource "version.txt"))))

(defn string []
  (format "%s/%s" (version) (build)))
