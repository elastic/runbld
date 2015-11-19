(ns runbld.post.test.junit
  (:require [clojure.java.io :as io]
            [net.cgrand.enlive-html :as x]))

(defn testcase-meta [xml]
  (-> xml
      (x/select [[(x/attr= :errors)
                  (x/attr= :failures)
                  (x/attr= :tests)]])
      first
      :attrs
      (update :errors   #(Integer/parseInt %))
      (update :failures #(Integer/parseInt %))
      (update :tests    #(Integer/parseInt %))))

(defn content-text [xml]
  (apply str (map x/text xml)))

(defn failed-testcase [xml]
  (merge (:attrs xml)
         {:stacktrace (content-text (:content xml))}))

(defn failed-testcases [xml]
  (map failed-testcase
       (x/select xml [[:testcase (x/has [:failure])]])))

(defn make-failure-report [xml]
  (let [meta (select-keys
              (testcase-meta xml)
              [:name :errors :failures :tests])]
    (when (pos? (:failures meta))
      (assoc meta
             :testcases
             (failed-testcases xml)))))

(defn find-failures [dir]
  (->> dir
       io/file
       file-seq
       (map #(.getCanonicalPath %))
       (map str)
       (filter #(re-find #"TEST-.*\.xml$" %))
       (map #(str "file://" %))
       (map #(java.net.URL. %))
       (map x/xml-resource)
       (map make-failure-report)
       (filter identity)))
