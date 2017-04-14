(ns runbld.results
  (:require [clojure.spec :as s]
            [runbld.schema])
  (:require [elasticsearch.document :as es.doc]
            [elasticsearch.connection.http :as es.conn]
            [runbld.store :as store]))

(def T
  (name runbld.schema/DocType))

(defn gradle-went-wrong [conn idx build-id]
  (let [q {:query
           {:bool
            {:must [{:match {:log "what went wrong"}}
                    {:match {:build-id build-id}}]}}
           :size 1}
        res (-> (es.doc/search conn idx T {:body q})
                :hits :hits first :_source)]
    #_(clojure.pprint/pprint
       ['went-wrong res])
    res))

(defn gradle-went-wrong-line [conn idx build-id]
  (when-let [ord (-> (gradle-went-wrong conn idx build-id) :ord :total)]
    ord))

(defn gradle-failed-task [conn idx build-id]
  (let [went-wrong-line (gradle-went-wrong-line conn idx build-id)
        q {:query
           {:bool
            {:must
             [{:match {:build-id build-id}}
              {:match {:stream :stderr}}
              {:match {:log "execution failed"}}
              {:range
               {:ord.total
                {:gte (inc went-wrong-line)}}}]}}
           :sort {:ord.total :asc}
           :size 1}
        res (es.doc/search conn idx {:body q})]
    (when-let [log (-> res :hits :hits first :_source :log)]
      (let [[_ taskname] (re-find #"'([^']+)'" log)]
        taskname))))

(defn gradle-failed-task-begin [conn idx build-id task]
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

(defn gradle-failed-task-end [conn idx build-id task]
  (when task
    (let [q {:query
             {:bool
              {:must
               [{:match {:build-id build-id}}
                #_{:match {:stream :stderr}}
                {:match {:log task}}
                {:match {:log "completed"}}]}}
             :size 1}
          res (es.doc/search conn idx {:body q})]
      (-> res :hits :hits first :_source :ord :total))))

(defn gradle-failure-lines [conn idx build-id]
  (let [task (gradle-failed-task conn idx build-id)
        start-line (gradle-failed-task-begin conn idx build-id task)
        end-line (gradle-failed-task-end conn idx build-id task)]
    (when (and task start-line end-line)
      (let [q {:query
               {:bool
                {:must
                 [{:match {:build-id build-id}}
                  {:range
                   {:ord.total {:gte start-line
                                :lte end-line}}}]}}
               :size 500
               :sort {:ord.total :asc}}
            res (es.doc/search conn idx {:body q})]
        (->> (-> res :hits :hits)
             (map :_source)
             (map :log))))))
