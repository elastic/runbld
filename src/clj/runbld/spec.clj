(ns runbld.spec
  (:require [clojure.spec :as s])
  (:import (elasticsearch.connection Connection)))

(s/def :runbld.store/index-name string?)

(s/def :runbld.store/url
  (s/and string?
         #(.startsWith % "http")))

(s/def :runbld.store/conn #(instance? Connection %))
(s/def :runbld.store/http-opts map?)
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
