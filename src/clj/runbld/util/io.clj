(ns runbld.util.io
  (:require [schema.core :as s])
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

(defn prepend-path
  "An absolute path-safe combinator.

    runbld.util.io> (prepend-path \"/tmp\" \"foo\")
    \"/tmp/foo\"
    runbld.util.io> (prepend-path \"/tmp\" \"/foo\")
    \"/foo\"
    runbld.util.io>
  "
  [dir basename]
  (if (.isAbsolute (file basename))
    basename
    (str (file dir basename))))

(defn reader [& args]
  (apply jio/reader args))

(defn resolve-resource [path]
  (if-let [tmpl (jio/resource path)]
    tmpl
    (if (.exists (file path))
      path
      (throw+ {:error ::resource-not-found
               :msg (format "cannot find %s" path)}))))

(defn os []
  (-> (System/getProperties)
      (get "os.name")
      .toUpperCase
      symbol))

(defn which-bin []
  (case (os)
    LINUX "which"
    WINDOWS "where.exe"))

(defn which [name]
  (let [res (sh/sh (which-bin) name)]
    (when (zero? (:exit res))
      (.trim (:out res)))))

(s/defn readlink
  [path :- s/Str]
  (-> path
      file
      .toPath
      (.toRealPath
       (into-array java.nio.file.LinkOption []))
      str))

(s/defn resolve-binary :- s/Str
  [name :- s/Str]
  (readlink (which name)))

(defmacro with-tmp-source [bindings body]
  (let [f (first bindings)
        src (second bindings)]
    `(let [~f (java.io.File/createTempFile "runbld-java-" ".clj")]
       (.deleteOnExit ~f)
       (spit ~f ~src)
       (let [res# ~body]
         (.delete ~f)
         res#))))

(defn tmp-dir [dir prefix]
  (.toFile
   (java.nio.file.Files/createTempDirectory
    (.toPath (file dir))
    prefix
    (into-array
     java.nio.file.attribute.FileAttribute []))))

(defmacro with-tmp-dir [bindings & body]
  `(let [d# ~((bindings 1) 0)
         pre# ~((bindings 1) 1)
         ~(bindings 0) (tmp-dir d# pre#)]
     (try
       ~@body
       (finally
         #_(rmdir-r
            ~(bindings 0))))))
