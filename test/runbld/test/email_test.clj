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

(use-fixtures :once schema.test/validate-schemas)

(defn run [args]
  (let [opts (opts/parse-args args)
        res (apply main/-main args)
        idx (-> res :store-result :addr :index)
        t (-> res :store-result :addr :type)
        id (-> res :store-result :addr :id)
        build-doc (store/get (-> opts :es :conn) idx t id)]
    [opts res build-doc]))

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
      (with-redefs [io/log (fn [& x] (prn x))
                    mail/send-message (fn [conn msg]
                                        (reset! email msg))]
        (git/with-tmp-repo [d "tmp/git/email-failures"]
          (io/run "rsync" "-a" "test/repo/java/some-errors/" d)
          (let [[opts res build-doc]
                (run
                  (conj
                   ["-c" "test/config/main.yml"
                    "-j" "elastic+foo+master"
                    "-d" d]
                   (if (opts/windows?)
                     "test/fail.bat"
                     "test/fail.bash")))]
            (is (= 1 (:exit-code res)))
            (is (= 1 (-> build-doc :process :exit-code)))
            (is (= 2 (count (store/get-failures opts (:id build-doc)))))
            (is (.contains (-> @email :body first :content)
                           "com.example.AppTest <b>testBad</b>")
                (with-out-str
                  (clojure.pprint/pprint @email)))))))))

(deftest log
  (testing "log present"
    (let [email (atom [])]
      (with-redefs [io/log (fn [& x] (prn x))
                    mail/send-message (fn [conn msg]
                                        (reset! email msg))]
        (git/with-tmp-repo [d "tmp/git/email-log"]
          (testing "no gradle task"
            (let [out (java.io.StringWriter.)
                  [opts res build-doc]
                  (binding [*out* out
                            *err* out]
                    (run
                      (conj
                       ["-c" "test/config/main.yml"
                        "-j" "elastic+foo+master"
                        "-d" d]
                       (if (opts/windows?)
                         "test/fail-gradle-no-task.bat"
                         "test/fail-gradle-no-task.bash"))))]
              (is (= 1 (-> build-doc :process :exit-code)))
              (is (.contains (-> @email :body first :content)
                             "Cannot expand ZIP")
                  (-> @email :body first :content))))
          (testing "with gradle task"
            (let [out (java.io.StringWriter.)
                  [opts res build-doc]
                  (binding [*out* out
                            *err* out]
                    (run
                      (conj
                       ["-c" "test/config/main.yml"
                        "-j" "elastic+foo+master"
                        "-d" d]
                       (if (opts/windows?)
                         "test/fail-gradle-with-task.bat"
                         "test/fail-gradle-with-task.bash"))))]
              (is (= 1 (-> build-doc :process :exit-code)))
              (is (.contains (-> @email :body first :content)
                             ":core:integTest")
                  (-> @email :body first :content))
              (is (= 2 (count (:body @email)))
                  (pr-str @email)))))))))
