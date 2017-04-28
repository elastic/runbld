(ns runbld.results
  (:require [clojure.string :as str]
            [elasticsearch.connection.http :as es.conn]
            [elasticsearch.document :as es.doc]
            [elasticsearch.scroll :as es.scroll]
            [runbld.results.gradle :as gradle]))

(defn summary [conn idx build-id]
  (when-let [ww (gradle/went-wrong-lines conn idx build-id)]
    (let [lines (gradle/failure-lines conn idx build-id)
          _ (prn (count lines))
          summary (if (>= 50 (count lines))
                    (str/join "\n" (concat ww ["---"] lines))
                    ww)]
      {:summary summary
       :lines lines})))

(defn count-log [conn idx build-id]
  (let [q {:query
           {:match {:build-id build-id}}}
        res (es.doc/count conn idx {:body q})]
    res))

(defn log-lines [conn idx build-id]
  (let [q {:query {:match {:build-id build-id}}
           :size 500
           :sort {:ord.total :asc}}]
    (map :_source (es.scroll/scroll conn {:index idx :body q}))))
