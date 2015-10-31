(ns runbld.opts
  (:require [clojure.tools.cli :as cli]
            [runbld.version :as version]
            [slingshot.slingshot :refer [throw+]]))

(def opts
  [["-v" "--version" "Print version"]])

(defn parse-args [args]
  (let [{:keys [options arguments summary errors]
         :as parsed-opts} (cli/parse-opts args opts)]
    (when (pos? (count errors))
      (throw+ {:error ::parse-error
               :msg (with-out-str
                      (doseq [err errors]
                        (println err)))}))

    (when (:version options)
      (throw+ {:help ::version
               :msg (version/version)}))

    (when (not (= 1 (count arguments)))
      (throw+ {:help ::usage
               :msg (format "runbld %s\nusage: rundmc /path/to/script.bash"
                            (version/version))}))

    ;; for now, just attach the shell script from Jenkins
    (assoc options :scriptfile (first arguments))))
