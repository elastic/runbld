(ns runbld.test.opts-test
  (:require [clojure.test :refer :all]
            [schema.test])
  (:require [clj-time.core :as t]
            [runbld.io :as io]
            [runbld.opts :as opts]
            [runbld.java :as java] :reload-all))

(use-fixtures :once schema.test/validate-schemas)

(deftest basic
  (let [java-home (:home (java/jvm-facts))]
    (is (= {:program (if (opts/windows?) "CMD.EXE" "zsh")
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
                              "-p" "zsh"
                              "/path/to/script.zsh"]))))))

(deftest profile1
  (let [java-home (:home (java/jvm-facts))]
    (is (= {:from "override@example.com"
            :to "override@example.com"}
           (-> ["-c" "test/config/opts.yml"
                "-j" "test,foo,master"
                "-p" "zsh"
                "--java-home" java-home
                "/path/to/script.zsh"]
               opts/parse-args
               :email
               (select-keys [:from :to]))))))
