(ns runbld.env
  (:require
   [clojure.walk]
   [runbld.schema :refer :all]
   [schema.core :as s]))

(def get-env
  (memoize
   (fn
     ([]
      (clojure.walk/keywordize-keys
       (into {} (System/getenv))))
     ([k]
      ((get-env) k)))))

