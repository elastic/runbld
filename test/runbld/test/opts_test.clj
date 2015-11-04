(ns runbld.test.opts-test
  (:require [clojure.test :refer :all])
  (:require [clj-time.core :as t]
            [runbld.opts :as opts] :reload-all))

(deftest config-file
  (is (= {:es.index.build "FOO",
          :scriptfile "/path/to/script.bash"}
         (select-keys (:opts
                       (opts/parse-args ["-c" "test/runbld.yaml"
                                         "--es.index.build" "FOO"
                                         "/path/to/script.bash"]))
                      [:es.index.build
                       :scriptfile]))))

(deftest date-expansion
  (is (= {:es.index.build (str "foo-" (t/year (t/now)))}
         (select-keys (:opts
                       (opts/parse-args ["--es.index.build" "'foo'-yyyy"
                                         "/path/to/script.bash"]))
                      [:es.index.build]))))
