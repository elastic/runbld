(ns runbld.util.date
  (:require [clj-time.coerce :as c]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [slingshot.slingshot :refer [throw+]]))

(defn expand
  ([fmt]
   (expand fmt (t/now)))
  ([fmt d]
   (try
     (let [formatter (f/formatter fmt)]
       (f/unparse formatter d))
     (catch IllegalArgumentException e
       (throw+ {:error ::pattern
                :msg (format
                      (str "invalid joda formatter pattern [%s]\n"
                           "see http://www.joda.org/joda-time/key_format.html")
                      fmt)})))))

(defn yyyymmdd-hhmmss
  ([]
   (yyyymmdd-hhmmss (t/now)))
  ([d]
   (let [formatter (f/formatter "yyyyMMddHHmmss")]
     (f/unparse formatter d))))

(defn ms-to-iso
  ([]
   (ms-to-iso (t/now)))
  ([ms]
   (f/unparse
    (f/formatters :date-time)
    (c/from-long ms))))

(defn date-to-iso
  ([judate]
   (str
    (c/from-date judate))))
