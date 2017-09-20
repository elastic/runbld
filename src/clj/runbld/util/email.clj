(ns runbld.util.email
  (:require
   [clojure.string :as str]
   [schema.core :as s]))

(s/defn obfuscate-addr :- s/Str
  [addr :- s/Str]
  (str/replace addr
               (re-pattern "(.*?)@([^.]+\\.)?(.)[^.]+\\.([^.]+)$")
               "$1@$3***.$4"))
