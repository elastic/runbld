(ns runbld.results
  (:require [clojure.string :as str]
            [runbld.results.gradle :as gradle]))

(defn summary [conn idx build-id]
  (when-let [ww (gradle/went-wrong-lines conn idx build-id)]
    (let [lines (gradle/failure-lines conn idx build-id)
          _ (prn (count lines))
          summary (if (>= 50 (count lines))
                    (str/join "\n" (concat ww ["---"] lines))
                    ww)]
      {:summary summary
       :lines lines})))
