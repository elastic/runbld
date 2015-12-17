(ns runbld.scheduler)

(defprotocol Scheduler
  (as-map [_] "All the data")
  (build-url [_] "Landing page")
  (console-url [_] "Log")
  (extra-info [_] "Extra fields to create")
  (tags [_] "Build tags")
  (vendor [_] "Scheduler type"))
