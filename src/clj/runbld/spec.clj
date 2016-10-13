(ns runbld.spec
  (:require [clojure.spec :as s]))

(s/def ::string string?)
(s/def ::hash string?)
(s/def ::version-info (s/keys :req-un [::string ::hash]))

(s/def ::index-name string?)
