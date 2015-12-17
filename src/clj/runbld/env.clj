(ns runbld.env
  (:require [runbld.schema :refer :all]
            [schema.core :as s]))

(s/defn wrap-env :- OptsStage3
  [proc :- clojure.lang.IFn]
  (fn [opts]
    (proc (assoc opts :env (into {} (System/getenv))))))
