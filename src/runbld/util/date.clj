(ns runbld.util.date
  (:require [clj-time.coerce :as c]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clojure.string :as str]
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

(defn long-to-iso [num]
  (str
   (c/from-long num)))

;; Thanks Devin Humbert!
;; https://www.cromulentbits.com/clojure-readable-time-duration/
(defn human-duration
  [secs]
  (let [isecs (int secs)
        days (quot isecs 86400)
        days-r (rem isecs 86400)
        hours (quot days-r 3600)
        hours-r (rem days-r 3600)
        minutes (quot hours-r 60)
        seconds (rem hours-r 60)]
    (str/join
     " "
     (filter #(not (str/blank? %))
             (conj []
                   (when (> days 0) (str days "d"))
                   (when (> hours 0) (str hours "h"))
                   (when (> minutes 0) (str minutes "m"))
                   (when (> seconds 0) (str seconds "s")))))))
