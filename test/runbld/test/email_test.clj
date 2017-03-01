(ns runbld.test.email-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer :all]
            [postal.core :as mail]
            [runbld.io :as io]
            [runbld.main :as main]
            [runbld.notifications.email :as email]
            [runbld.opts :as opts]
            [runbld.store :as store]
            [runbld.vcs.git :as git]
            [schema.test]
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

(deftest attached
  (testing "failures present"
    (let [email (atom [])]
      (with-redefs [io/log (fn [& _] :noconsole)
                    mail/send-message (fn [& args]
                                        (swap! email concat args))]
        (git/with-tmp-repo [d "tmp/git/email-failures"]
          (io/run "rsync" "-a" "test/repo/java/some-errors/" d)
          (let [args (conj
                      ["-c" "test/config/main.yml"
                       "-j" "elastic+foo+master"
                       "-d" d]
                      (if (opts/windows?)
                        "test/fail.bat"
                        "test/fail.bash"))
                opts (opts/parse-args args)
                res (apply main/-main args)
                build-doc (store/get (-> opts :es :conn)
                                     (-> res :store-result :addr))]
            (is (= 1 (:exit-code res)))
            (is (= 1 (-> build-doc :process :exit-code)))
            (is (= 2 (count (store/get-failures opts (:id build-doc)))))))))))
