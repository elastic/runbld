(ns runbld.util.date
  (:require
   [clj-time.coerce :as c]
   [clj-time.core :as t]
   [clj-time.format :as f]
   [clojure.string :as str]
   [slingshot.slingshot :refer [throw+]])
  (:import
   (org.joda.time Period)))

(defn now []
  (t/now))

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

(defn from-iso [iso]
  (f/parse (f/formatter :date-time) iso))

(defn iso-diff-secs [iso1 iso2]
  (t/in-seconds (t/interval iso1 iso2)))

;; Thanks Devin Humbert!
;; https://www.cromulentbits.com/clojure-readable-time-duration/
(defn human-duration
  "Takes a number of seconds and returns a human readable version of
  the duration.  e.g. 1232352 -> '14d 6h 19m 12s'.  Currently only
  handles up to a granularity in days."
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
                   (str seconds "s"))))))

(defn duration-in-seconds
  "This is the other side of 'human-duration' and will convert output
  like '14d 6h 19m 12s' to 1232352.  It handles granularity up to
  daily (again, like 'human-duration')"
  [duration-str]
  (-> duration-str
      str/lower-case
      (str/replace #"\s" "")
      (str/replace #"^([0-9]+d)?(.*)$"
                   (fn [[_ p t]]
                     (format "P%s%s"
                             (if-not (empty? p)
                               p
                               "")
                             (if-not (empty? t)
                               (str "T" t)
                               ""))))
      (Period/parse)
      (.toStandardDuration)
      (.getMillis)
      (/ 1000)))
