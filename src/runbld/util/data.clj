(ns runbld.util.data
  (:refer-clojure :exclude [map?])
  (:require [clojure.walk :refer [postwalk]]))

(defn map? [x]
  (or (instance? clojure.lang.IPersistentMap x)
      (instance? java.util.Map x)))

;; http://dev.clojure.org/jira/browse/CLJ-1468
(defn deep-merge
  "Like merge, but merges maps recursively."
  {:added "1.7"}
  [& maps]
  (if (every? map? maps)
    (apply merge-with deep-merge maps)
    (last maps)))

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level."
  {:added "1.7"}
  [f & maps]
  (apply
   (fn m [& maps]
     (if (every? map? maps)
       (apply merge-with m maps)
       (apply f maps)))
   maps))

(defn keywordize-keys
  "clojure.walk/keywordize-keys breaks when a map is actually a
  j.u.LinkedHashMap, which is what happens with interop, in our case
  Yaml#load()"
  [m]
  (with-redefs [clojure.core/map? map?]
    (clojure.walk/keywordize-keys m)))
