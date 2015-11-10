(ns runbld.util.io
  (:require [clojure.java.io :as jio]
            [clojure.java.shell :as sh]))

(defn run [& args]
  (let [cmd (map str args)
        res (apply sh/sh cmd)]
    (assert (= 0 (:exit res)) (format "%s: %s"
                                      (pr-str cmd)
                                      (:err res)))))

(defn rmdir-r [dir]
  (run "rm" "-r" dir))

(defn rmdir-rf [dir]
  (run "rm" "-rf" dir))

(defn mkdir-p [dir]
  (run "mkdir" "-p" dir))

(defn abspath [f]
  (.getCanonicalPath (jio/as-file f)))

(defn abspath-file [f]
  (jio/file (abspath f)))
