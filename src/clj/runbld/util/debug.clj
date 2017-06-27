(ns runbld.util.debug
  "The intent behind this debug ns is to provide a globally accessible
  logging function (i.e. one that doesn't rely on any state passed
  in).  So while this may appear to be gross b/c of the essentially
  global state that it maintains, it was done deliberately to prevent
  having to thread that state into all corners of the codebase."
  (:require
   [clj-time.core :as t]
   [clj-time.format :as f]
   [clojure.stacktrace :as stacktrace]
   [clojure.string :as string]
   [runbld.io :as io]
   [postal.core :as mail]
   [runbld.util.email :as email]
   [slingshot.slingshot :refer [try+ throw+]])
  (:import (java.util Date)))

(def date-format (f/formatters :date-time))

(def debug-log
  "The log is basically a vector of stuff that will eventually be
  coerced into a string and emailed."
  (atom []))

(defn reset
  "Reset the debug-log internal state.  Mostly this is used in tests."
  []
  (reset! debug-log []))

(defn get-log []
  @debug-log)

(defn print-log []
  (doseq [l @debug-log]
    (println l)))

(defn log* [ns form-meta & more]
  (let [[throwable msgs] (if (instance? Throwable (first more))
                           [(first more) (rest more)]
                           [nil more])]
    (swap! debug-log conj
           (format "[%s] [%s %s:%s] %s%s"
                   (f/unparse date-format (t/now))
                   ns
                   (:line form-meta "")
                   (:column form-meta "")
                   (apply print-str msgs)
                   (if throwable
                     (str "\n" (with-out-str
                                 (stacktrace/print-cause-trace throwable)))
                     "")))))

(defmacro log
  "Appends the message and throwable to the debug log, for output if
  an error occurs"
  {:arglists '([log-msg]
               [throwable log-msg]
               [log-msg & more-msgs]
               [throwable log-msg & more-msgs])}
  [& args]
  `(log* ~*ns* ~(meta &form) ~@args))

(defn send-log [{{:keys [to]} :debug
                 {:keys [from] :as email-cfg} :email
                 :keys [job-name]
                 :as opts}]
  (when to
    (io/log "Sending debug log to" (email/obfuscate-addr to))
    (mail/send-message
     email-cfg
     {:from from
      :to to
      :subject (str "runbld debug log for " job-name)
      :body [{:type "text/plain; charset=utf-8"
              :content (string/join "\n" @debug-log)}]})))

(defn with-debug-logging [proc opts]
  (try+
   (proc opts)
   (catch Object o
     (let [t (:throwable &throw-context)]
       (log t "Error caught in debug logger.")
       (when (-> opts :debug :to)
         (send-log opts))
       (throw+ t)))))
