(ns runbld.test.email-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [stencil.core :as mustache]))

(deftest render
  (let [ctx (edn/read-string
             (slurp "test/context.edn"))]
    (spit "/tmp/runbld-email.html"
          (mustache/render-file
           "templates/email.mustache.html" ctx))))
