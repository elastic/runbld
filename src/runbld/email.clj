(ns runbld.email
  (:refer-clojure :exclude [send])
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [clojure.string :as str]
            [postal.core :as mail]
            [runbld.store :as store]
            [runbld.util.date :as date]
            [runbld.util.io :as io]
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

(s/defn send* :- clojure.lang.IPersistentMap
  [conn    :- {s/Keyword s/Any}
   from    :- s/Str
   to      :- [s/Str]
   subject :- s/Str
   plain   :- s/Str
   html    :- (s/maybe s/Str)]
  (let [body [:alternative
              {:type "text/plain; charset=utf-8"
               :content plain}]
        body (if html
               (conj body
                     {:type "text/html; charset=utf-8"
                      :content html})
               body)]
    (mail/send-message
     conn
     {:from from
      :to to
      :subject subject
      :body body})))

(s/defn send :- clojure.lang.IPersistentMap
  [opts :- Opts
   ctx* :- EmailCtx]
  (let [ctx (merge
             (-> ctx*
                 (update :cmd #(str/join " " %1))
                 (update :args #(str/join " " %1))
                 (update :rcpt-to #(str/join ", " %1)))
             {:took-human (date/human-duration
                           (/ (:took ctx*) 1000))
              :stdout (slurp (:out-file ctx*))
              :stderr (slurp (:err-file ctx*))
              :commit-short (->> (:commit ctx*) (take 7) (apply str))})]
    ;; If needing to regenerate for render tests
    #_(spit "context.edn"
          (with-out-str
            (clojure.pprint/pprint
             (into (sorted-map) ctx))))
    (send* (opts :email)
           (-> opts :email :from)
           (split-addr (-> opts :email :to))
           (format "%s %s %s"
                   (ctx :status)
                   (ctx :build-name)
                   (ctx :commit-short))
           (mustache/render-string
            (slurp
             (io/resolve-resource
              (-> opts :email :template-txt)))
            ctx)
           (when (and (-> opts :email :template-html)
                      (not (-> opts :email :text-only)))
             (mustache/render-string
              (slurp
               (io/resolve-resource
                (-> opts :email :template-html)))
              ctx)))))

(defn maybe-send! [opts {:keys [index type id] :as addr}]
  (let [build-doc (store/get (-> opts :es :conn) addr)]
    (clojure.pprint/pprint build-doc)))
