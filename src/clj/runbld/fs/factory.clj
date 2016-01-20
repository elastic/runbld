(ns runbld.fs.factory
  (:require [runbld.schema :refer :all]
            [schema.core :as s]
            [slingshot.slingshot :refer [throw+]])
  (:require [runbld.io :as io]
            [runbld.fs :refer [FileSystem] :as fs]
            [runbld.util.data :as data])
  (:import (java.nio.file Files)))

(defrecord JavaFileSystem [jfs]
  FileSystem
  (fs-mountpoint [x]
    (-> x .jfs .name))

  (fs-type [x]
    (-> x .jfs .type))

  (fs-bytes-total [x]
    (-> x .jfs .getTotalSpace))

  (fs-bytes-free [x]
    (-> x .jfs .getUsableSpace))

  (fs-bytes-used [x]
    (let [tot (fs/fs-bytes-total x)
          free (fs/fs-bytes-free x)]
      (when (and tot free)
        (- tot free))))

  (fs-percent-free [x]
    (data/percent
     (fs/fs-bytes-free x)
     (fs/fs-bytes-total x)))

  (fs-percent-used [x]
    (data/percent
     (fs/fs-bytes-used x)
     (fs/fs-bytes-total x))))

(s/defn make-fs
  ([dir :- s/Str]
   (JavaFileSystem.
    (-> (io/file dir)
        .toPath
        Files/getFileStore))))
