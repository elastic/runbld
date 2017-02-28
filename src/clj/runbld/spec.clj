(ns runbld.spec
  (:require [clojure.future :refer :all]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [elasticsearch.connection.http :as es])
  (:import (elasticsearch.connection Connection)))

;; Assuming require [clojure.tools.logging :as log]
(Thread/setDefaultUncaughtExceptionHandler
 (reify Thread$UncaughtExceptionHandler
   (uncaughtException [_ thread ex]
     (locking #'clojure.pprint/pprint
       (clojure.pprint/pprint "Uncaught exception on" (.getName thread) ex)))))

(s/def :runbld.store/index-name
  (s/with-gen string?
    (fn []
      (gen/fmap #(.toLowerCase %)
                (gen/not-empty (gen/string-alphanumeric))))))

(s/def :runbld.store/url
  (s/with-gen (s/and string?
                     #(.startsWith % "http"))
    #(s/gen #{"http://localhost:9200"})))

(s/def :runbld.store/conn
  (s/with-gen (partial instance? Connection)
    #(s/gen #{(es/make {:url "http://localhost:9200"})})))

(s/def :runbld.store/http-opts
  (s/with-gen map?
    #(s/gen #{{:query-string ""}})))

(s/def :runbld.store/index-meta
  (s/with-gen (s/keys :opt-un [:runbld.store/settings
                               :runbld.store/mappings])
    #(s/gen #{{:settings
               {:index
                {:number_of_shards 1
                 :number_of_replicas 0}}}})))

(s/def :runbld.store/settings map?)
(s/def :runbld.store/mappings map?)
(s/def :runbld.store/build-index :runbld.store/index-name)
(s/def :runbld.store/failure-index :runbld.store/index-name)
(s/def :runbld.store/log-index :runbld.store/index-name)
(s/def :runbld.store/max-index-bytes integer?)
(s/def :runbld.store/bulk-timeout-ms int?)
(s/def :runbld.store/bulk-size int?)

(s/def :runbld.store/es-opts
  (s/keys :req-un [:runbld.store/url :runbld.store/http-opts]))

(s/def :runbld.store/opts
  (s/keys :req-un [:runbld.store/url
                   :runbld.store/http-opts
                   :runbld.store/build-index
                   :runbld.store/failure-index
                   :runbld.store/log-index
                   :runbld.store/max-index-bytes
                   :runbld.store/bulk-timeout-ms
                   :runbld.store/bulk-size]))

(s/def :runbld.store/build-doc-init
  (s/keys :req-un [:runbld.build/id
                   :runbld.build/build
                   :runbld.java/java
                   :runbld.system/sys
                   :runbld.version/version
                   :runbld.vcs/vcs]))
