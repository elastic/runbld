(ns runbld.util.data
  (:refer-clojure :exclude [bigdec])
  (:require
   [clojure.string :as str]
   [clojure.walk]
   [slingshot.slingshot :refer [throw+]]))

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

(defn bigdec
  "Set f to n places after the decimal."
  ([f]
   (when f
     (clojure.core/bigdec f)))
  ([f n]
   (when f
     (-> (clojure.core/bigdec f)
         (.setScale n BigDecimal/ROUND_HALF_UP)))))

(defn safe-div
  ([m n]
   (if (and m n (zero? m) (zero? n))
     1.0
     (when (and m n (pos? n))
       (float (/ m n)))))
  ([m n decimals]
   (bigdec (safe-div m n) decimals)))

(defn percent
  ([m n]
   (percent m n 3))
  ([m n decimals]
   (when-let [quot (safe-div m n)]
     (bigdec
      (* quot 100) decimals))))

(defn strip-trailing-slashes [s]
  (when s
    (if-let [[_ rst] (re-find #"^/+(.*)"
                              (->> s str/trim str/reverse))]
      (str/reverse rst)
      s)))

(defn as-int [value]
  (when value
    (cond
      (integer? value) value
      (and
       (string? value)
       (re-find #"\d+" value)) (Integer/parseInt value)
      :else (throw+
             {:type ::error
              :msg (format
                    "don't know how to make an integer out of: %s (%s)"
                    (pr-str value)
                    (type value))}))))
