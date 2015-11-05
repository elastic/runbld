(ns runbld.publish.email
  (:refer-clojure :exclude [send])
  (:require [postal.core :as mail]
            [stencil.core :as mustache]))

(defn send* [conn from to subject msg]
  (mail/send-message
   conn
   {:from from
    :to to
    :subject subject
    :body msg}))

(defn send [opts ctx]
  (send* (-> opts :opts :email)
         (ctx :mail-from)
         (ctx :rcpt-to)
         (format "%s %s"
                 (ctx :github-name)
                 (ctx :jenkins-number))
         (mustache/render-string
          (slurp (-> opts :opts :email :template))
          ctx)))
