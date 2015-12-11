(ns runbld.vcs.repo
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [clojure.java.io :as io]
            [runbld.vcs :as vcs])
  (:import (runbld.vcs.git GitRepo)))

(defn make-repo [dir]
  (cond
    (.isDirectory (io/file dir ".git")) (GitRepo. dir)

    :else (throw
           (Exception.
            (format (str
                     "%s: unknown repository type "
                     "(only know about git currently)")
                    dir)))))

(s/defn wrap-vcs-info :- OptsFinal
  [proc :- clojure.lang.IFn]
  (fn [opts]
    (proc
     (assoc opts
            :vcs (vcs/log-latest
                  (make-repo
                   (get-in opts [:process :cwd])))))))
