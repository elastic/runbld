(ns runbld.publish.email
  (:refer-clojure :exclude [send])
  (:require [clojure.string :as str]
            [postal.core :as mail]
            [runbld.opts :refer [Opts]]
            [runbld.util.date :as date]
            [runbld.util.io :as io]
            [schema.core :as s]
            [stencil.core :as mustache]))

(def Ctx
  {
   (s/required-key :branch-url       ) s/Str
   (s/required-key :build-name       ) s/Str
   (s/required-key :commit           ) s/Str
   (s/required-key :commit-desc      ) s/Str
   (s/required-key :commit-email     ) s/Str
   (s/required-key :commit-msg       ) s/Str
   (s/required-key :commit-name      ) s/Str
   (s/required-key :commit-time      ) s/Str
   (s/required-key :commit-url       ) s/Str
   (s/required-key :env              ) {s/Str s/Any}
   (s/required-key :exit-code        ) s/Num
   (s/required-key :id               ) s/Str
   (s/required-key :job-name         ) s/Str
   (s/required-key :out-bytes        ) s/Num
   (s/required-key :err-bytes        ) s/Num
   (s/required-key :out-file         ) s/Str
   (s/required-key :err-file         ) s/Str
   (s/required-key :mail-from        ) s/Str
   (s/required-key :org              ) s/Str
   (s/required-key :project          ) s/Str
   (s/required-key :project-url      ) s/Str
   (s/required-key :rcpt-to          ) [s/Str]
   (s/required-key :scriptfile       ) s/Str
   (s/required-key :start-millis     ) s/Num
   (s/required-key :status           ) s/Str
   (s/required-key :time-end         ) s/Str
   (s/required-key :time-start       ) s/Str
   (s/required-key :took             ) s/Num

   (s/optional-key :architecture     ) s/Str
   (s/optional-key :args             ) [s/Str]
   (s/optional-key :branch           ) s/Str
   (s/optional-key :cmd              ) [s/Str]
   (s/optional-key :cmd-source       ) s/Str
   (s/optional-key :cwd              ) s/Str
   (s/optional-key :end-millis       ) s/Num
   (s/optional-key :hardwaremodel    ) s/Str
   (s/optional-key :hostname         ) s/Str
   (s/optional-key :ipaddress        ) s/Str
   (s/optional-key :ipaddress6       ) s/Str
   (s/optional-key :jenkins-executor ) (s/maybe s/Str)
   (s/optional-key :jenkins-labels   ) (s/maybe s/Str)
   (s/optional-key :jenkins-node     ) (s/maybe s/Str)
   (s/optional-key :jenkins-number   ) (s/maybe s/Str)
   (s/optional-key :job-name-extra   ) s/Str
   (s/optional-key :kernelrelease    ) s/Str
   (s/optional-key :kernelversion    ) s/Str
   (s/optional-key :memorysize_mb    ) s/Str
   (s/optional-key :operatingsystem  ) s/Str
   (s/optional-key :operatingsystemrelease ) s/Str
   (s/optional-key :physicalprocessorcount ) s/Num
   (s/optional-key :processor0       ) s/Str
   (s/optional-key :processorcount   ) (s/cond-pre s/Str s/Num)
   (s/optional-key :profile-name     ) s/Str
   (s/optional-key :program          ) s/Str
   (s/optional-key :timezone         ) s/Str
   (s/optional-key :uptime_days      ) s/Num
   (s/optional-key :url              ) (s/maybe s/Str)
   (s/optional-key :workspace        ) s/Str
   })

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
   ctx* :- Ctx]
  (let [ctx (merge
             (-> ctx*
                 (update :cmd #(str/join " " %1))
                 (update :args #(str/join " " %1))
                 (update :rcpt-to #(str/join ", " %1)))
             {:took-human (date/human-duration
                           (/ (:took ctx*) 1000))
              :stdout (slurp (:out-file ctx*))
              :stderr (slurp (:err-file ctx*))})]
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
                   (ctx :commit))
           (mustache/render-string
            (slurp
             (io/resolve-resource
              (-> opts :email :template-txt)))
            ctx)
           (when (-> opts :email :template-html)
             (mustache/render-string
              (slurp
               (io/resolve-resource
                (-> opts :email :template-html)))
              ctx)))))
