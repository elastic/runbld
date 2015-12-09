(ns runbld.vcs.repo
  (:require [clojure.java.io :as io])
  (:import (runbld.vcs.git GitRepo)))

(defn make-repo [dir]
  (cond
    (.isDirectory (io/file dir ".git"))
    (runbld.vcs.git.GitRepo. dir)

    :else (throw
           (Exception.
            (format (str
                     "%s: unknown repository type "
                     "(only know about git currently)")
                    dir)))))
