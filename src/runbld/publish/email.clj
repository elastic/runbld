(ns runbld.publish.email
  (:refer-clojure :exclude [send])
  (:require [clojure.string :as str]
            [postal.core :as mail]
            [stencil.core :as mustache]))

(defn maybe-split-addr [s]
  (if (and (string? s) (.contains s ","))
    (->> (str/split s #",")
         (map #(.trim %)))
    s))

(defn send* [conn from to subject msg]
  (mail/send-message
   conn
   {:from from
    :to to
    :subject subject
    :body msg}))

(defn send [opts ctx]
  (send* (opts :email)
         (ctx :mail-from)
         (ctx :rcpt-to)
         (format "%s %s"
                 (ctx :github-name)
                 (ctx :jenkins-number))
         (mustache/render-string
          (slurp (-> opts :email :template))
          ctx)))
