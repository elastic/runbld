(ns runbld.test.opts-test
  (:require [clojure.test :refer :all]
            [schema.test])
  (:require [clj-time.core :as t]
            [runbld.opts :as opts] :reload-all))

(use-fixtures :once schema.test/validate-schemas)

(deftest basic
  (is (= {:program "zsh"
          :args ["-x"]
          :inherit-exit-code true
          :scriptfile "/path/to/script.zsh"
          :cwd (System/getProperty "user.dir")
          :stdout ".stdout.log"
          :stderr ".stderr.log"}
         (:process
          (opts/parse-args ["-c" "test/config/opts.yml"
                            "-j" "test,foo,master"
                            "-p" "zsh"
                            "/path/to/script.zsh"])))))

(deftest profile1
  (is (= {:from "override@example.com"
          :to "override@example.com"}
         (-> ["-c" "test/config/opts.yml"
               "-j" "test,foo,master"
               "-p" "zsh"
               "/path/to/script.zsh"]
              opts/parse-args
              :email
              (select-keys [:from :to])))))
