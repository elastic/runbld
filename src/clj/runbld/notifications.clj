(ns runbld.notifications
  (:require [runbld.schema :refer :all]
            [pallet.thread-expr :refer [when->]]
            [clojure.string :as str]
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
      (when-> (:test build)
        (update-in [:test :failed-testcases] (partial take 10))
        (update-in [:test :failed-testcases] (partial sort-by :class))
        (update-in [:test :failed-testcases] (partial sort-by :test)))))
