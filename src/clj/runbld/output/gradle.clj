(ns runbld.output.gradle
  (:require [clojure.spec :as s]
            [runbld.schema])
  (:require [elasticsearch.connection.http :as es.conn]
            [elasticsearch.document :as es.doc]
            [elasticsearch.indices :as indices]
            [runbld.store :as store]))

(def T
  (name runbld.schema/DocType))

(defn went-wrong-start [conn idx build-id]
  (let [q {:query
           {:bool
            {:must [{:match {:log "what"}}
                    {:match {:log "went"}}
                    {:match {:log "wrong"}}
                    {:match {:stream :stderr}}
                    {:match {:build-id build-id}}]}}
           :size 1}
        res (-> (es.doc/search conn idx T {:body q})
                :hits :hits first :_source)]
    (when (= (:log res) "* What went wrong:")
      (-> res :ord))))

(defn went-wrong-end [conn idx build-id start]
  (when-let [wwl (:stream
                  (went-wrong-start conn idx build-id))]
    (let [q {:query
             {:bool
              {:must [{:match {:build-id build-id}}
                      {:match {:log "try"}}
                      {:match {:stream :stderr}}
                      {:range {:ord.stream {:gte wwl}}}]}}
             :size 1}
          res (-> (es.doc/search conn idx T {:body q})
                  :hits :hits first :_source)]
      (when (= (:log res) "* Try:")
        (-> res :ord)))))

(defn went-wrong-lines [conn idx build-id]
  (when-let [wwl-start (:stream
                        (went-wrong-start conn idx build-id))]
    (when-let [wwl-end (:stream
                        (went-wrong-end conn idx build-id wwl-start))]
      (let [q {:query
               {:bool
                {:must [{:match {:build-id build-id}}
                        {:match {:stream :stderr}}
                        {:range {:ord.stream {:gte wwl-start
                                              :lt (dec wwl-end)}}}]}}
               :sort {:ord.stream :asc}
               :size 20}
            res (es.doc/search conn idx T {:body q})]
        (->> res
             :hits
             :hits
             (map #(get-in % [:_source :log])))))))

(defn failed-task [conn idx build-id]
  (when-let [wwl (:stream
                  (went-wrong-start conn idx build-id))]
    (let [q {:query
             {:bool
              {:must
               [{:match {:build-id build-id}}
                {:match {:log "execution failed"}}
                {:match {:stream :stderr}}
                {:range {:ord.stream {:gte wwl}}}]}}
             :sort {:ord.total :asc}
             :size 3}
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
                {:match {:log task}}
                {:match {:log "failed"}}]}}
             :size 1
             :sort {:ord.total :asc}}
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
