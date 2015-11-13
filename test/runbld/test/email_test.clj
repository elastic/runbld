(ns runbld.test.email-test
  (:require [clojure.core.cache]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [stencil.core :as mustache]
            [stencil.loader]))

(stencil.loader/set-cache
 (clojure.core.cache/ttl-cache-factory {} :ttl 0))

(deftest render
  (spit "/tmp/runbld-email.html"
        (mustache/render-file
         "templates/email.mustache.html"
         (edn/read-string
          (slurp "test/context.edn")))))
