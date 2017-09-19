(ns runbld.opts-test
  (:require [clojure.test :refer :all]
            [runbld.test-support :as ts]
            [schema.test])
  (:require [clj-time.core :as t]
            [runbld.io :as io]
            [runbld.opts :as opts]
            [runbld.java :as java] :reload-all))

(use-fixtures :once schema.test/validate-schemas)
(use-fixtures :each ts/redirect-logging-fixture)

(deftest basic
  (let [java-home (:home (java/jvm-facts))
        program (if (opts/windows?) "CMD.EXE" "zsh")]
    (is (= {:program program
            :args (if (opts/windows?) ["/C"] ["-x"])
            :inherit-exit-code true
            :inherit-env false,
            :scriptfile "/path/to/script.zsh"
            ;; The case will resolve differently on Windows between
            ;; user.dir and io/abspath (c: vs C:)
            :cwd (io/abspath
                  (System/getProperty "user.dir"))
            :stdout ".stdout.log"
            :stderr ".stderr.log"
            :output ".output.log"
            :env {:JAVA_HOME java-home}}
           (:process
            (opts/parse-args ["-c" "test/config/opts.yml"
                              "-j" "test,foo,master"
                              "--java-home" java-home
                              "-p" program
                              "/path/to/script.zsh"]))))))

(deftest profile1
  (let [java-home (:home (java/jvm-facts))
        program (if (opts/windows?) "CMD.EXE" "zsh")]
    (is (= {:from "override@example.com"
            :to "override@example.com"}
           (-> ["-c" "test/config/opts.yml"
                "-j" "test,foo,master"
                "-p" program
                "--java-home" java-home
                "/path/to/script.zsh"]
               opts/parse-args
               :email
               (select-keys [:from :to]))))))

(deftest stdin
  (let [java-home (:home (java/jvm-facts))
        prog "l33t code"
        scriptfile (-> ["-c" "test/config/opts.yml"
                        "-j" "test,foo,master"
                        "--java-home" java-home
                        (opts/make-script "-" (java.io.StringReader. prog))]
                       opts/parse-args
                       :process
                       :scriptfile)]
    (is (re-find #".*stdin.*" scriptfile))
    (is (= prog (slurp scriptfile)))))
