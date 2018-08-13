(ns runbld.notifications.email
  (:refer-clojure :exclude [send])
  (:require
   [clojure.string :as str]
   [postal.core :as mail]
   [runbld.io :as io]
   [runbld.notifications :as n]
   [runbld.schema :refer :all]
   [runbld.store :as store]
   [runbld.util.debug :as debug]
   [runbld.util.email :as email]
   [runbld.vcs :as vcs]
   [schema.core :as s]
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

(defn attachment-filename [failure]
  (format "%s-%s-%s-%s"
          (:build-id failure)
          (:class failure)
          (first (str/split (:test failure) #" " 2))
          (entropy)))

(def Attachment
  {:type s/Keyword
   :content java.io.File
   :content-type s/Str})

(s/defn attach-failure :- Attachment
  [failure :- StoredFailure]
  (let [f (io/make-tmp-attachment (attachment-filename failure) ".txt"
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
   reply-to :- s/Str
   subject  :- s/Str
   plain    :- s/Str
   html     :- (s/maybe s/Str)
   attachments :- [s/Any]]
  (let [body
        (concat
         [:mixed
          (concat
           [:alternative]
           ;; With multipart/alternative, the earlier the part, the lower
           ;; the priority, so place plain text first and the HTML last
           [{:type "text/plain; charset=utf-8"
             :content plain}]
           (when-not (empty? html)
             [{:type "text/html; charset=utf-8"
               :content html}]))]
         attachments)]
    (mail/send-message
     conn
     {:from from
      :to to
      :reply-to reply-to
      :subject subject
      :body body})))

(s/defn render :- s/Str
  [tmpl :- s/Str
   ctx :- EmailCtx]
  (mustache/render-string
   (slurp (io/resolve-resource tmpl)) ctx))

(s/defn send :- s/Any
  [opts
   ctx :- EmailCtx]
  ;; If needing to regenerate for render tests
  #_(spit "test/context.edn"
          (with-out-str
            (clojure.pprint/pprint
             (into (sorted-map) ctx))))
  (let [rcpts (split-addr (-> opts :email :to))]
    ((opts :logger) "MAILING:" (str/join ", "
                                         (map email/obfuscate-addr rcpts)))
    (let [out (with-out-str
                (clojure.pprint/pprint
                 (send* (opts :email)
                        (-> opts :email :from)
                        rcpts
                        (-> opts :email :reply-to)
                        (-> ctx :email :subject)
                        (render (-> opts :email :template-txt) ctx)
                        (when (and (-> opts :email :template-html)
                                   (not (-> opts :email :text-only)))
                          (render (-> opts :email :template-html) ctx))
                        (concat
                         (map attach-failure (:failures ctx))
                         (when-let [a (attach-log
                                       (:id ctx) (-> ctx :log :lines))]
                           [a])))))
          msg (if (.contains out "SUCCESS")
                "Mail sent.  Output: "
                "Mail failed to send.  Output: ")]
      ((opts :logger) (str msg out)))))

(s/defn make-context :- EmailCtx
  [opts build failures]
  (-> (n/make-context opts build failures)
      (update-in [:email :to] #(str/join ", " %))
      (assoc-in [:email :subject]
                (format "%s %s %s%s - %s"
                        (-> build :process :status)
                        (-> build :build :org-project-branch)
                        (-> build :vcs :commit-short)
                        (let [x (-> build :build :job-name-extra)]
                          (if-not (empty? x)
                            (str " " x)
                            ""))
                        (:id opts)))))

(s/defn send? :- s/Bool
  [email-opts :- OptsEmail
   build :- StoredBuild]
  (and
   (not (:disable email-opts))
   (pos?
    (-> build :process :exit-code))))

(defn maybe-send! [opts {:keys [index type id]}]
  (let [logger (:logger opts)
        build-doc (store/get (-> opts :es :conn) index type id)
        failure-docs (store/get-failures opts (:id build-doc))]
    (if (send? (-> opts :email) build-doc)
      (send
       (update-in opts
                  [:email :port]
                  #(if (string? %)
                     (try
                       (Integer/parseInt %))
                     %))
       (make-context opts build-doc failure-docs))
      (logger "NO MAIL GENERATED"))))

(s/defn send-email :- {:email-result s/Any
                       s/Keyword s/Any}
  [opts :- {:store-result {:addr {s/Keyword s/Any}
                           :url s/Str
                           :build-doc {s/Keyword s/Any}}
            :email OptsEmail
            s/Keyword s/Any}]
  (debug/log "Send email stage")
  (assoc opts :email-result
         (io/try-log (maybe-send! opts (-> opts :store-result :addr)))))
