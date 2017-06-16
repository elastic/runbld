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
  :repositories {"jenkins" "http://repo.jenkins-ci.org/releases"}
  :global-vars {*warn-on-reflection* false}
  :min-lein-version "2.0.0"
  :exclusions [org.clojure/clojure
               prismatic/schema]
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]
  :javac-options ["-Xlint:unchecked"]
  :jvm-opts ["-server"]
  :dependencies [[clj-time "0.13.0"]
                 [circleci/clj-yaml "0.5.5"]
                 [com.draines/postal "2.0.2"
                  :exclusions
                  [ ;; Newer one comes from clj-http
                   commons-codec
                   ]]
                 [instaparse "1.4.5"]

                 ;; logging
                 [org.clojure/tools.logging "0.3.1"]
                 [log4j/log4j "1.2.17"]

                 [com.palletops/thread-expr "1.3.0"]
                 [elastic/elasticsearch-clojure "0.99.5"
                  :exclusions [prismatic/schema]]
                 [enlive "1.1.6"]
                 [environ "1.1.0"]
                 [listora/again "0.1.0"]
                 [org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/core.async "0.3.442"]
                 [org.clojure/tools.cli "0.3.5"]

                 ;; Use 1.7 because the Jenkins plugin is pinned to 1.7
                 [org.tmatesoft.svnkit/svnkit "1.8.14"]
                 [prismatic/schema "1.1.5"]
                 [robert/bruce "0.8.0"]
                 [slingshot "0.12.2"]
                 [stencil "0.5.0"]]
  :injections [(require 'clojure.pprint)]
  :profiles {:dev {:env {:dev "true"}}
             :package {:plugins [[elastic/lein-bin "0.3.6"]]
                       :bin {:bootclasspath false}}}
  :plugins [[lein-environ "1.0.3"]]
  :aliases {"package" ["with-profile" "package" "bin"]}
  :aot :all
  :main runbld.main
  :test-selectors {:default (complement :integration)
                   :integration :integration
                   :all (constantly true)})
