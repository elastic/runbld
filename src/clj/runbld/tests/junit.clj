(ns runbld.tests.junit
  (:require [clojure.java.io :as io]
            [clojure.set :refer [rename-keys]]
            [clojure.string :as str]
            [net.cgrand.enlive-html :as x]
            [runbld.io :as rio]
            [runbld.schema :refer :all]
            [runbld.util.debug :as debug]
            [schema.core :as s])
  (:import (org.xml.sax SAXParseException)))

(defn maybe-update [m ks f & x]
  (into m (for [[k v] (select-keys m ks)]
            [k (apply f v x)])))

(defn testcase-meta [xml]
  (-> xml
      (x/select [[(x/attr= :errors)
                  (x/attr= :failures)
                  (x/attr= :tests)]])
      first
      :attrs
      (rename-keys {:skip :skipped})
      (maybe-update [:errors :failures :tests :skipped]
                    #(Integer/parseInt %))))

(defn content-text [xml]
  (apply str (map x/text xml)))

(s/defn collect-errors-and-failures
  ([class :- s/Str
    test :- s/Str
    xml :- XML]
   (merge
    {:error-type (name (:tag xml))
     :class class
     :test test
     :stacktrace (content-text (:content xml))
     :summary (format "%s %s %s"
                      (.toUpperCase (name (:tag xml)))
                      (-> class
                          (str/split #"\.")
                          last)
                      test)}
    (:attrs xml))))

(defn failed-testcase [xml]
  (let [{:keys [classname name]} (:attrs xml)]
    (map (partial collect-errors-and-failures classname name)
         (x/select xml [#{:failure :error}]))))

(defn failed-testcases [xml]
  (mapcat failed-testcase
          (x/select xml [:testcase])))

(defn make-failure-report [xml]
  (let [meta (select-keys
              (testcase-meta xml)
              [:name :errors :failures :tests :skipped])]
    (assoc meta
           :failed-testcases
           (failed-testcases xml))))

(s/defn combine-failure-reports :- TestSummary
  [total testsuite]
  (try
    (-> total
        (update :errors   + (:errors    testsuite))
        (update :failures + (:failures  testsuite))
        (update :tests    + (:tests     testsuite))
        (update :skipped  + (:skipped   testsuite))
        (update :failed-testcases concat (:failed-testcases testsuite)))
    (catch Throwable t
      (clojure.pprint/pprint testsuite)
      (throw t))))

(def default-failure-report
  {:errors 0
   :failures 0
   :tests 0
   :skipped 0})

(defn merge-default [m]
  (merge default-failure-report m))

(defn find-failures [dir]
  (->> (rio/find-files dir #"TEST-.*\.xml$")
       (map #(try
               (x/xml-resource %)
               (catch SAXParseException e
                 (rio/log "Failed to parse" (rio/abspath %)
                          "because of" (.getMessage e))
                 (debug/log e "Full trace")
                 nil)))
       (remove nil?)
       (mapcat #(x/select % [[(x/tag= :testsuite)]]))
       (map make-failure-report)
       (filter identity)
       (map merge-default)
       (reduce combine-failure-reports {:errors 0
                                        :failures 0
                                        :tests 0
                                        :skipped 0
                                        :failed-testcases []})))
