(ns runbld.version
  (:require [clojure.java.io :as io]))

(defn build []
  (if-let [b (io/resource "build.txt")]
    (.trim (slurp b))
    "NOBUILD" #_"For fresh tests"))

(defn version []
  (.trim (slurp (io/resource "version.txt"))))

(defn string []
  (format "%s/%s" (version) (build)))
