(ns runbld.test.opts-test
  (:require [clojure.test :refer :all])
  (:require [clj-time.core :as t]
            [runbld.opts :as opts] :reload-all))

(deftest config-file
  (is (= {:program "zsh"
          :args ["-x"]
          :scriptfile "/path/to/script.zsh"
          :cwd (System/getProperty "user.dir")}
         (:process
          (opts/parse-args ["-c" "test/runbld.yaml"
                            "--program" "zsh"
                            "/path/to/script.zsh"])))))
