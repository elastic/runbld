(ns runbld.test.email-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [runbld.notifications.email :as email]
            [stencil.core :as mustache]))

;; This is a waste until we dynamically generate test/context.edn
#_(deftest render
  (let [f "tmp/runbld-email.html"]
    (spit f
          (mustache/render-string
           (slurp
            (io/resource "templates/email.mustache.html"))
           (edn/read-string
            (slurp "test/context.edn"))))
    (is (= 1572 (count (slurp f))))))

(deftest obfuscation
  (is (= "foo@b***.dom"
         (email/obfuscate-addr "foo@bar.dom")))
  (is (= "foo@b***.dom"
         (email/obfuscate-addr "foo@bar.baz.dom")))
  (is (= "foo@q***.dom"
         (email/obfuscate-addr "foo@bar.quux.dom"))))
