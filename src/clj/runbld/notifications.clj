(ns runbld.notifications
  (:require [clojure.string :as str]
            [pallet.thread-expr :refer [when-> when-not->]]
            [runbld.build :as build]
            [runbld.schema :refer :all]
            [runbld.results :as results]
            [runbld.util.date :as date]
            [schema.core :as s]))

(defn strip-out-runbld [src]
  (->> src
       java.io.StringReader.
       clojure.java.io/reader
       line-seq
       (remove #(re-find #"^#!.*runbld" %))
       (str/join "\n")))

(s/defn make-context :- NotifyCtx
  [opts build failures]
  (let [test-results? (boolean
                       (or
                        (and (number? (-> build :test :errors))
                             (pos? (-> build :test :errors)))
                        (and (number? (-> build :test :failures))
                         (pos? (-> build :test :failures)))))
        last-success (build/find-build
                      opts (-> build :build :last-success :id))
        results-summary (when-not test-results?
                          (results/summary
                           (-> opts :es :conn) (-> opts :es :log-index-search)
                           (:id build)))]
    (-> build
        (update-in [:process :cmd] #(str/join " " %))
        (update-in [:process :cmd-source] strip-out-runbld)
        (update-in [:process :args] #(str/join " " %))
        (assoc-in [:process :took-human]
                  (date/human-duration
                   (/ (-> build :process :took) 1000)))
        (update-in [:version :hash]
                   #(->> % (take 7) (apply str)))
        (assoc-in [:process :failed]
                  (= "FAILURE" (-> build :process :status)))
        (assoc-in [:failures] failures)
        (when-> test-results?
          (assoc-in [:tests] true)
          (update-in [:test :failed-testcases] (partial take 10))
          (update-in [:test :failed-testcases] (partial sort-by :class))
          (update-in [:test :failed-testcases] (partial sort-by :test)))
        (when-> last-success
          (assoc-in [:build :last-success :age]
                    (date/human-duration
                     (date/iso-diff-secs
                      (date/from-iso
                       (-> last-success :process :time-end))
                      (date/from-iso
                       (-> build :process :time-start)))))
          (assoc-in [:build :last-success :time-end]
                    (-> last-success :process :time-end)))
        (when-> results-summary
          (assoc-in [:log :present] true)
          (update-in [:log] merge results-summary))
        (when-not-> (or results-summary test-results?)
          (assoc-in [:no-output] true)))))
