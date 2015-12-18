(ns runbld.test.email-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [stencil.core :as mustache]))

(deftest render
  (let [f "tmp/runbld-email.html"]
    (spit f
          (mustache/render-string
           (slurp
            (io/resource "templates/email.mustache.html"))
           (edn/read-string
            (slurp "test/context.edn"))))
    (is (= 1572 (count (slurp f))))))
