(ns runbld.notifications.email
  (:refer-clojure :exclude [send])
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [clojure.string :as str]
            [postal.core :as mail]
            [runbld.store :as store]
            [runbld.io :as io]
            [runbld.notifications :as n]
            [runbld.vcs :as vcs]
            [stencil.core :as mustache]))

(defn split-addr [s]
  (vec
   (cond
     (and
      (string? s)
      (.contains s ",")) (->> (str/split s #",")
                              (map #(.trim %)))
     (string? s) [s]
     (sequential? s) s)))

(defn entropy []
  (->> (java.util.UUID/randomUUID)
       str
       (take 4)
       (apply str)
       .toUpperCase))

(def Attachment
  {:type s/Keyword
   :content java.io.File
   :content-type s/Str})

(s/defn attach-failure :- Attachment
  [failure :- StoredFailure]
  (let [f (io/make-tmp-attachment
           (format "%s-%s-%s-%s"
                   (:build-id failure)
                   (:class failure)
                   (:test failure)
                   (entropy))
           ".txt"
           :del? true)]
    (spit f (:stacktrace failure))
    {:type :attachment
     :content f
     :content-type "text/plain"}))

(s/defn attach-log :- (s/maybe Attachment)
  [id :- s/Str
   log-lines :- [s/Str]]
  (when (pos? (count log-lines))
    (let [f (io/make-tmp-attachment
             (format "%s-log-%s" id (entropy))
             ".txt"
             :del? true)]
      (spit f (->> log-lines (interpose "\n") (apply str)))
      {:type :attachment
       :content f
       :content-type "text/plain"})))

(s/defn send* :- s/Any
  [conn     :- {s/Keyword s/Any}
   from     :- s/Str
   to       :- [s/Str]
   subject  :- s/Str
   plain    :- s/Str
   html     :- (s/maybe s/Str)
   attachments :- [s/Any]]
  (let [body (concat
              [(if html
                 {:type "text/html; charset=utf-8"
                  :content html}
                 {:type "text/plain; charset=utf-8"
                  :content plain})]
              attachments)]
    (mail/send-message
     conn
     {:from from
      :to to
      :subject subject
      :body body})))

(s/defn obfuscate-addr :- s/Str
  [addr :- s/Str]
  (str/replace addr
               (re-pattern "(.*?)@([^.]+\\.)?(.)[^.]+\\.([^.]+)$")
               "$1@$3***.$4"))

(s/defn render :- s/Str
  [tmpl :- s/Str
   ctx :- EmailCtx]
  (mustache/render-string
   (slurp (io/resolve-resource tmpl)) ctx))

(s/defn send :- s/Any
  [opts :- MainOpts
   ctx :- EmailCtx]
  ;; If needing to regenerate for render tests
  #_(spit "test/context.edn"
          (with-out-str
            (clojure.pprint/pprint
             (into (sorted-map) ctx))))
  (let [rcpts (split-addr (-> opts :email :to))]
    ((opts :logger) "MAILING:" (str/join ", "
                                         (map obfuscate-addr rcpts)))
    ((opts :logger)
     (with-out-str
       (clojure.pprint/pprint
        (send* (opts :email)
               (-> opts :email :from)
               rcpts
               (-> ctx :email :subject)
               (render (-> opts :email :template-txt) ctx)
               (when (and (-> opts :email :template-html)
                          (not (-> opts :email :text-only)))
                 (render (-> opts :email :template-html) ctx))
               (concat
                (map attach-failure (:failures ctx))
                (when-let [a (attach-log (:id ctx) (-> ctx :log :lines))]
                  [a]))))))))

(s/defn make-context :- EmailCtx
  [opts build failures]
  (-> (n/make-context opts build failures)
      (update-in [:email :to] #(str/join ", " %))
      (assoc-in [:email :subject]
                (format "%s %s %s%s"
                        (-> build :process :status)
                        (-> build :build :org-project-branch)
                        (-> build :vcs :commit-short)
                        (if-let [x (-> build :build :job-name-extra)]
                          (str " " x) "")))))

(s/defn send? :- s/Bool
  [email-opts :- OptsEmail
   build :- StoredBuild]
  (and
   (not (:disable email-opts))
   (pos?
    (-> build :process :exit-code))))

(defn maybe-send! [opts {:keys [index type id]}]
  (let [build-doc (store/get (-> opts :es :conn) index type id)
        failure-docs (store/get-failures opts (:id build-doc))]
    (if (send? (-> opts :email) build-doc)
      (send opts (make-context opts build-doc failure-docs))
      ((opts :logger) "NO MAIL GENERATED"))))
