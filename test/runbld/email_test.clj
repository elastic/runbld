(ns runbld.email-test
  (:require
   [clojure.edn :as edn]
   [clojure.test :refer :all]
   [postal.core :as mail]
   [runbld.io :as io]
   [runbld.main :as main]
   [runbld.notifications.email :as email]
   [runbld.opts :as opts]
   [runbld.store :as store]
   [runbld.test-support :as ts]
   [runbld.util.email :as email-util]
   [runbld.vcs.git :as git]
   [schema.test]
   [stencil.core :as mustache]))

(use-fixtures :each ts/redirect-logging-fixture)
(use-fixtures :once
  schema.test/validate-schemas
  ts/dont-die-fixture)

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
         (email-util/obfuscate-addr "foo@bar.dom")))
  (is (= "foo@b***.dom"
         (email-util/obfuscate-addr "foo@bar.baz.dom")))
  (is (= "foo@q***.dom"
         (email-util/obfuscate-addr "foo@bar.quux.dom"))))

(defn find-contents [body]
  (->> (tree-seq sequential? identity body)
       (filter #(and (map? %) (not (= :attachment (:type %)))))
       (map :content)))

(deftest attached
  (testing "attachment filenames are trimmed correctly"
    (with-redefs [email/entropy (constantly "ABCD")]
      (let [test-msg "thisTestFailed"
            failure-1 {:build-id "1234-4321"
                       :class "some.java.class.SingletonFactoryObserver"
                       :test test-msg}
            failure-2 (assoc failure-1
                             :test
                             (str test-msg " {but there more text, too}"))]
        (is (= (email/attachment-filename failure-1)
               (email/attachment-filename failure-2))))))
  (testing "failures present"
    (let [email (atom [])]
      (with-redefs [mail/send-message (fn [conn msg]
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
            (let [body (:body @email)
                  contents (find-contents body)]
              (is (= 2 (count contents))
                  body)
              (doseq [content contents]
                (is (re-find #"com\.example\.AppTest.*testBad" content))))))))))

(deftest log
  (testing "log present"
    (let [email (atom [])]
      (with-redefs [mail/send-message (fn [conn msg]
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
              (let [body (:body @email)
                    contents (find-contents body)]
                (is (= 2 (count contents)) body)
                (doseq [content contents]
                  (is (.contains content "Cannot expand ZIP")))
                (is (= 2 (count body))
                    (pr-str @email)))))
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
              (let [body (:body @email)
                    contents (find-contents body)]
                (is (= 2 (count contents)) body)
                (doseq [content contents]
                  (is (.contains content ":core:integTest")))
                (is (= 3 (count body))
                    (pr-str @email))))))))))

(deftest reproduce-with
  (let [email (atom nil)
        out (java.io.StringWriter.)]
    (binding [*out* out
              *err* out]
      (with-redefs [mail/send-message (fn [conn msg]
                                        (reset! email msg))]
        (git/with-tmp-repo [d "tmp/git/reproduce-with"]
          (run (conj
                ["-c" "test/config/main.yml"
                 "-j" "elastic+foo+master"
                 "-d" d]
                (if (opts/windows?)
                  "test/fail-gradle-with-task.bat"
                  "test/fail-gradle-with-task.bash")))
          (is (re-find #"REPRODUCE WITH: gradle :core:integTest"
                       (pr-str @email))
              "There should have been a reproduce with section"))))))

(deftest reply-to
  (let [email (atom [])
        out (java.io.StringWriter.)]
    (with-redefs [mail/send-message (fn [conn msg]
                                      (reset! email msg))]
      (git/with-tmp-repo [d "tmp/git/email-reply-to"]
        (binding [*out* out
                  *err* out]
          (run (conj ["-c" "test/config/main.yml"
                      "-j" "elastic+foo+master"
                      "-d" d]
                     (if (opts/windows?)
                       "test/fail.bat"
                       "test/fail.bash"))))
        (is (= (:reply-to @email) "replyto@example.com"))))))
