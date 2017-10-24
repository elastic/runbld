(defproject co.elastic/runbld
  (try (-> "resources/version.txt" slurp .trim)
       (catch java.io.FileNotFoundException _
         (println "WARNING! Missing resources/version.txt,"
                  "falling back to 0.0.1-SNAPSHOT")
         "0.0.1-SNAPSHOT"))
  :description "They wrap"
  :url "https://github.com/elastic/runbld"
  :license {:name "Apache License, Version 2.0"
            :url "https://www.apache.org/licenses/LICENSE-2.0"}
  :global-vars {*warn-on-reflection* false}
  :min-lein-version "2.0.0"
  :exclusions [org.clojure/clojure
               prismatic/schema]
  :source-paths ["src/clj"]
  :clean-targets ["target" "tmp"]
  :java-source-paths ["src/java"]
  :javac-options ["-Xlint:unchecked"]
  :jvm-opts ["-server"]
  :dependencies [[clj-time "0.14.0"]
                 [circleci/clj-yaml "0.5.6"]
                 [com.draines/postal "2.0.2"
                  ;; Newer one comes from clj-http
                  :exclusions [commons-codec]]
                 [instaparse "1.4.7"]

                 ;; fact gathering
                 [com.github.oshi/oshi-json "3.4.4"]

                 ;; logging
                 [org.slf4j/slf4j-nop "1.7.25"]

                 [com.palletops/thread-expr "1.3.0"]
                 [elastic/elasticsearch-clojure "0.99.6"
                  :exclusions [prismatic/schema]]
                 [enlive "1.1.6"]
                 [environ "1.1.0"]
                 [listora/again "0.1.0"]
                 [org.clojure/clojure "1.9.0-beta1"]
                 [org.clojure/core.async "0.3.443"]
                 [org.clojure/tools.cli "0.3.5"]

                 [prismatic/schema "1.1.6"]
                 [robert/bruce "0.8.0"]
                 [slingshot "0.12.2"]
                 [stencil "0.5.0"]]
  :injections [(require 'clojure.pprint)]
  :profiles {:dev {:env {:dev "true"}}
             :package {:plugins [[elastic/lein-bin "0.3.6"]]
                       :bin {:bootclasspath false}}}
  :plugins [[lein-environ "1.0.3"]]
  :aliases {"package" ["with-profile" "package" "bin"]
            "test!" ["do" "clean," "test"]}
  :aot :all
  :main runbld.main
  :test-selectors {:default (complement :integration)
                   :integration :integration
                   :all (constantly true)})
