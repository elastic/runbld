(ns runbld.version
  (:require [clojure.java.io :as io]))

(defn version []
  (.trim (slurp (io/resource "version.txt"))))
