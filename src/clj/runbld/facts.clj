(ns runbld.facts
  (:require [schema.core :as s]
            [slingshot.slingshot :refer [throw+]])
  (:require [runbld.io :as io]))

(defprotocol Facter
  (arch            [_])
  (cpu-type        [_])
  (cpus            [_])
  (cpus-physical   [_])
  (facter-provider [_])
  (facter-version  [_])
  (hostname        [_])
  (ip4             [_])
  (ip6             [_])
  (kernel-name     [_])
  (kernel-release  [_])
  (kernel-version  [_])
  (model           [_])
  (os              [_])
  (os-version      [_])
  (os-family       [_])
  (ram-mb          [_])
  (ram-gb          [_])
  (ram-bytes       [_])
  (timezone        [_])
  (uptime-days     [_])
  (uptime-secs     [_])
  (uptime          [_])
  (virtual         [_])

  (raw             [_]))

(s/defn ram-mb-from-slash-proc :- s/Num
  [facts :- {s/Keyword s/Any}]
  (let [meminfo "/proc/meminfo"]
    (if (.exists (io/file meminfo))
      (let [memtotal-raw (:out (io/run "fgrep" "MemTotal" meminfo))
            [_ kb] (or (re-find #"^MemTotal: +(\d+) kB" memtotal-raw)
                       (throw+ {:type ::error
                                :msg (format
                                      "can't get memtotal from meminfo:\n%s"
                                      (with-out-str
                                        (println memtotal-raw)
                                        (clojure.pprint/pprint facts)))}))]
        (float (/ (Integer/parseInt kb) 1024)))
      (throw+ {:type ::error
               :msg (format "can't get memory info from:\n%s"
                            (with-out-str
                              (clojure.pprint/pprint facts)))}))))
