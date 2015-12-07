(ns runbld.env)

(defn wrap-env [proc]
  (fn [opts]
    (proc (assoc opts :env (into {} (System/getenv))))))
