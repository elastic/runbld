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
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.cli "0.3.3"]
                 [slingshot "0.12.2"]]
  :profiles {:package {:plugins [[lein-bin "0.3.4"]]
                       :bin {:bootclasspath true}
                       :aot [runbld.main]
                       :main runbld.main}}
  :aliases {"package" ["with-profile" "package" "bin"]}
  :test-selectors {:default  #(not (:integration %))
                   :integration :integration
                   :all (constantly true)})
