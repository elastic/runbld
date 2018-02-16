(ns runbld.slack-test
  (:require
   [clojure.test :refer :all]
   [runbld.io :as io]
   [runbld.notifications.slack :as slack]))

(deftest message-rendering
  (let [ctx {:process {:failed false}
             :build {:url "http://some-ci.domain.com/job/141"
                     :console-url "http://some-ci.domain.com/job/141/console"
                     :number 141
                     :project "project"
                     :job-name-extra "EXTRA EXTRA"
                     :branch "master"}
             :vcs {:branch-url "http://github.com/org/repo/master"
                   :commit-url "http://github.url/org/repo/commitwhatever"
                   :commit-short "deadbeef"}}
        opts {:slack {:template
                      (io/abspath (io/resolve-resource
                                   "templates/slack.mustache.json"))}
              :process {:failed true}}
        msg (slack/render opts ctx)
        msg-without-extra (slack/render
                           opts (assoc-in ctx [:build :job-name-extra] ""))
        msg-with-failure (slack/render
                          opts (assoc-in ctx [:process :failed] true))]
    (is (re-find #"http://some-ci.domain.com/job/141/console" msg))
    (is (re-find #"EXTRA EXTRA" msg))
    (is (re-find #"http://some-ci.domain.com/job/141/console"
                 msg-without-extra))
    (is (not (re-find #"\"text\":" msg-without-extra)))
    (is (not (re-find #"FAILURE" msg)))
    (is (re-find #"FAILURE" msg-with-failure))))
