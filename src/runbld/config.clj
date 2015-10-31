(ns runbld.config
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [runbld.util.data :refer [deep-merge-with]]
            [slingshot.slingshot :refer [throw+]]))

(def confs
  [{:location "runbld.edn"
    :type :resource}
   {:location "/etc/runbld.edn"
    :type :file}
   {:location "runbld.edn"
    :type :file}])

#_(defn wrap-create-connections
    "For each key in the config that's part of GROUP, that looks like an
  ES endpoint, create a connection based on it and add it to the
  config, named like ES.GROUP.CONN."
    [cfg]
    (let [attach-connections
          (fn [[url-key group]]
            (when url-key
              (let [k (keyword url-key)
                    url (cfg k)]
                {(keyword (format "es.%s.conn" group))
                 (es/make (merge
                           {:url url}
                           (cfg (keyword (format "es.%s.conn-opts" cfg)))))})))]
      (->> (keys cfg)
           (map name)
           (map #(re-find #"^es\.(.*)\.url$" %))
           (map attach-connections)
           (reduce merge cfg))))

(defmulti get-contents (fn [conf] (:type conf)))

(defmethod get-contents :resource [{:keys [location]}]
  (slurp (io/resource location)))

(defmethod get-contents :file [{:keys [location]}]
  (let [f (io/file location)]
    (if (.isFile f)
      (slurp location))))

(defn load-config [conf]
  (when-let [contents (get-contents conf)]
    (edn/read-string contents)))

(def config-map
  (apply deep-merge-with merge (map load-config confs)))

(defn make-config [m]
  (fn
    ([key]
     (if-let [v (m key)]
       v
       (throw+ {:type ::no-key
                :key key})))
    ([key default]
     (or (m key) default))))

(def config
  (make-config config-map))
