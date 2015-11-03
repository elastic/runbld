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
  :exclusions [org.clojure/clojure]
  :dependencies [[clj-time "0.11.0"]
                 [com.draines/postal "1.11.3"]
                 [org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.cli "0.3.3"]
                 [org.elasticsearch/elasticsearch-clojure "0.99.0-SNAPSHOT"]
                 [org.yaml/snakeyaml "1.16"]
                 [slingshot "0.12.2"]]
  :injections [(require 'clojure.pprint)]
  :profiles {:dev {}
             :package {:plugins [[lein-bin "0.3.4"]]
                       :bin {:bootclasspath true}
                       :injections [(require 'runbld.opts)
                                    (alter-var-root
                                     #'runbld.opts/*dev*
                                     (constantly false))]}}
  :aliases {"package" ["with-profile" "package" "bin"]}
  :aot [runbld.main]
  :main runbld.main
  :test-selectors {:default  #(not (:integration %))
                   :integration :integration
                   :all (constantly true)})
