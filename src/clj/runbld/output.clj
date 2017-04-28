(ns runbld.output
  (:require [clojure.string :as str]
            [elasticsearch.connection.http :as es.conn]
            [elasticsearch.document :as es.doc]
            [elasticsearch.scroll :as es.scroll]
            [runbld.output.gradle :as gradle]))

(defn summary
  "Return a standard object that can be passed to a notification
  context.  It's very gradle-centric right now, but maybe not
  forever?"
  [conn idx build-id]
  (when-let [ww (gradle/went-wrong-lines conn idx build-id)]
    (let [lines (gradle/failure-lines conn idx build-id)
          summary (str/join
                   "\n" (if (>= 50 (count lines))
                          (concat ww ["\n---\n"] lines)
                          ww))]
      {:summary summary
       :lines lines})))

;; Helpers for general log research, they aren't used in production
;; runbld at the moment.

(defn log-lines-query [id]
  {:match {:build-id id}})

(defn log-lines
  "Lazily get log lines from a build in their original total order,
  but /in reverse/.  Why reverse?  In practice that's almost always
  what we want, and keeps us from having to non-lazily reverse out of
  band."
  [conn idx build-id]
  (let [q {:query (log-lines-query build-id)
           :size 500
           :sort {:ord.total :desc}}]
    (map :_source (es.scroll/scroll conn {:index idx :body q}))))

(defn count-log
  "Like log-lines, but only counts."
  [conn idx build-id]
  (let [q {:query (log-lines-query build-id)}
        res (es.doc/count conn idx {:body q})]
    res))

(defn to-bash
  "Format a log line as a bash line that /echoes/ a log line to a file
  descriptor. :mindblown:"
  [log]
  (format
   "echo '%s' >%s" (str/replace (:log log) "'" "'\\''")
   (case (:stream log)
     "stdout" "&1"
     "stderr" "&2")))

(defn make-a-bash
  ([conn idx build-id filename]
   (make-a-bash conn idx build-id filename 500))
  ([conn idx build-id filename n]
   (->> (log-lines conn idx build-id)
        (take n)
        reverse
        (map to-bash)
        (str/join "\n")
        (spit filename))))
