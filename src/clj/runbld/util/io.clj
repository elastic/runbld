(ns runbld.util.io
  (:require [clojure.java.io :as jio]
            [clojure.java.shell :as sh]
            [slingshot.slingshot :refer [throw+]]))

(defn run [& args]
  (let [cmd (map str args)
        res (apply sh/sh cmd)]
    (assert (= 0 (:exit res)) (format "%s: %s"
                                      (pr-str cmd)
                                      (:err res)))
    res))

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

(defn file [& args]
  (apply jio/file args))

(defn resolve-resource [path]
  (if-let [tmpl (jio/resource path)]
    tmpl
    (if (.exists (file path))
      path
      (throw+ {:error ::resource-not-found
               :msg (format "cannot find %s" path)}))))

(defn capture-to-writer [wtr]
  (fn [line]
    (binding [*out* wtr]
      (println line)
      (flush))))

(defn spit-stream [^java.io.InputStream input
                   processors]
  (let [bs (atom 0)]
    (doseq [line (line-seq (jio/reader input))]
      (doseq [processor processors]
        (processor line))
      (swap! bs + (inc ;; for the newline
                   (count (.getBytes line)))))
    @bs))

(defn spit-process [out-is out-wtr
                    err-is err-wtr]
  [(future
     (spit-stream out-is
                  [(capture-to-writer *out*)
                   (capture-to-writer out-wtr)]))
   (future
     (spit-stream err-is
                  [(capture-to-writer *err*)
                   (capture-to-writer err-wtr)]))])

(defn which [name]
  (let [res (sh/sh "which" name)]
    (when (zero? (:exit res))
      (.trim (:out res)))))

(defmacro with-tmp-source [bindings body]
  (let [f (first bindings)
        src (second bindings)]
    `(let [~f (java.io.File/createTempFile "runbld-java-" ".clj")]
       (.deleteOnExit ~f)
       (spit ~f ~src)
       (let [res# ~body]
         (.delete ~f)
         res#))))
