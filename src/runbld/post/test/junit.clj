(ns runbld.post.test.junit
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
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
      (update :tests    #(Integer/parseInt %))
      (update :skipped  #(Integer/parseInt %))))

(defn content-text [xml]
  (apply str (map x/text xml)))

(defn collect-errors-and-failures [class test xml]
  (merge
   {:error-type (:tag xml)
    :class class
    :test test
    :stacktrace (content-text (:content xml))
    :summary (format "%s %s %s"
                     (.toUpperCase (name (:tag xml)))
                     (-> class
                         (str/split #"\.")
                         last)
                     test)}
   (:attrs xml)))

(defn failed-testcase [xml]
  (let [{:keys [classname name]} (:attrs xml)]
    (map (partial collect-errors-and-failures classname name)
         (x/select xml [#{:failure :error :skipped}]))))

(defn failed-testcases [xml]
  (mapcat failed-testcase
          (x/select xml [:testcase])))

(defn make-failure-report [xml]
  (let [meta (select-keys
              (testcase-meta xml)
              [:name :errors :failures :tests :skipped])]
    (when (or (pos? (:failures meta))
              (pos? (:errors meta)))
      (assoc meta
             :testcases
             (failed-testcases xml)))))

(defn combine-failure-reports [total testsuite]
  (try
    (-> total
        (update :errors + (:errors testsuite))
        (update :failures + (:failures testsuite))
        (update :tests + (:tests testsuite))
        (update :skipped + (:skipped testsuite))
        (update :testcases concat (:testcases testsuite)))
    (catch Throwable t
      (clojure.pprint/pprint testsuite)
      (throw t))))

(defn find-failures [dir]
  (when dir
    (->> dir
         io/file
         file-seq
         (map #(.getCanonicalPath %))
         (map str)
         (filter #(re-find #"TEST-.*\.xml$" %))
         (map #(str "file://" %))
         (map #(java.net.URL. %))
         (map x/xml-resource)
         (pmap make-failure-report)
         (filter identity)
         (reduce combine-failure-reports {:errors 0
                                          :failures 0
                                          :tests 0
                                          :skipped 0
                                          :testcases []}))))
