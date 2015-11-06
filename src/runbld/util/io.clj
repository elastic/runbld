(ns runbld.util.io
  (:require [clojure.java.io :as jio]
            [clojure.java.shell :as sh]))

(defn run [& args]
  (let [res (apply sh/sh (map str args))]
    (assert (= 0 (:exit res)) (:err res))))

(defn rmdir-r [dir]
  (run "rm" "-r" dir))

(defn mkdir-p [dir]
  (run "mkdir" "-p" dir))

