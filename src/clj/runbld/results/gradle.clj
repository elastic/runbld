(ns runbld.results.gradle
  (:require [clojure.spec :as s]
            [runbld.schema])
  (:require [elasticsearch.connection.http :as es.conn]
            [elasticsearch.document :as es.doc]
            [elasticsearch.indices :as indices]
            [runbld.store :as store]))

(def T
  (name runbld.schema/DocType))

(defn went-wrong-line [conn idx build-id]
  (let [q {:query
           {:bool
            {:must [{:match {:log "what went wrong"}}
                    {:match {:build-id build-id}}]}}
           :size 1}
        res (-> (es.doc/search conn idx T {:body q})
                :hits :hits first :_source)]
    #_(clojure.pprint/pprint
       ['went-wrong res])
    (-> res :ord :total)))

(defn went-wrong [conn idx build-id]
  (when-let [wwl (went-wrong-line conn idx build-id)]
    (let [q {:query
             {:bool
              {:must [{:match {:build-id build-id}}
                      {:match {:ord.total (inc wwl)}}]}}
             :size 1}
          res (-> (es.doc/search conn idx T {:body q})
                  :hits :hits first :_source :log)]
      #_(clojure.pprint/pprint
         ['went-wrong wwl res])
      res)))

(defn failed-task [conn idx build-id]
  (when-let [went-wrong-line (went-wrong-line conn idx build-id)]
    (let [q {:query
             {:bool
              {:must
               [{:match {:build-id build-id}}
                #_{:match {:stream :stderr}}
                {:match {:log "execution failed"}}
                {:range
                 {:ord.total
                  {:gte (inc went-wrong-line)}}}]}}
             :sort {:ord.total :asc}
             :size 1}
          res (es.doc/search conn idx {:body q})]
      (when-let [log (-> res :hits :hits first :_source :log)]
        (let [[_ taskname] (re-find #"'([^']+)'" log)]
          taskname)))))

(defn failed-task-begin [conn idx build-id task]
  (when task
    (let [q {:query
             {:bool
              {:must
               [{:match {:build-id build-id}}
                #_{:match {:stream :stderr}}
                {:match {:log task}}
                {:match {:log "started"}}]}}
             :size 1}
          res (es.doc/search conn idx {:body q})]
      (-> res :hits :hits first :_source :ord :total))))

(defn failed-task-end [conn idx build-id task]
  (when task
    (let [q {:query
             {:bool
              {:must
               [{:match {:build-id build-id}}
                #_{:match {:stream :stderr}}
                {:match {:log task}}
                {:match {:log "failed"}}]}}
             :size 1}
          res (es.doc/search conn idx {:body q})]
      (-> res :hits :hits first :_source :ord :total))))

(defn failure-lines [conn idx build-id]
  (when-let [task (failed-task conn idx build-id)]
    (let [start-line (failed-task-begin conn idx build-id task)
          end-line (failed-task-end conn idx build-id task)]
      (when (and task start-line end-line)
        (let [q {:query
                 {:bool
                  {:must
                   [{:match {:build-id build-id}}
                    {:range
                     {:ord.total {:gte start-line
                                  :lte end-line}}}]}}
                 :size 3000
                 :sort {:ord.total :desc}}
              res (es.doc/search conn idx {:body q})]
          (->> (-> res :hits :hits)
               (map :_source)
               (map :log)
               reverse))))))
