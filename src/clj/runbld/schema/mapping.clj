(ns runbld.schema.mapping
  (:refer-clojure :exclude [boolean long double])
  (:require [clojure.walk :as walk]
            [schema.core :as s]))

(defn map-entry? [x]
  (or (instance? java.util.Map$Entry x)
      (and (vector? x)
           (= 2 (count x))
           (keyword? (first x)))))

(defn entry-val [x]
  (cond
    (instance? java.util.Map$Entry x) (val x)
    :else (second x)))

(defn entry-key [x]
  (cond
    (instance? java.util.Map$Entry x) (key x)
    :else (first x)))

(defn has-key? [m k]
  (some #{k} (keys m)))

(defn entry-has-subkey? [e k]
  (and
   (map-entry? e)
   (map? (entry-val e))
   (has-key? (entry-val e) k)))

(defn has-inner-object? [x]
  (entry-has-subkey? x :properties))

(defn entry [k v]
  (first {k v}))

(defn schema-walk [form]
  (let [make-key (fn [x]
                   ((if (-> x entry-val (has-key? :schema-required))
                      (if (-> x entry-val :schema-required)
                        s/required-key s/optional-key)
                      s/required-key)
                    (entry-key x)))
        f (fn [x]
            (cond
              (has-inner-object? x)
              (entry
               (make-key x) (schema-walk
                             (:properties
                              (entry-val x))))

              (entry-has-subkey? x :schema-type)
              (entry
               (make-key x)
               (-> x entry-val :schema-type))

              :else x))]
    (walk/postwalk f form)))

(defn mapping-walk [form]
  (let [f (fn [x]
            (cond
              (map? x) (dissoc x :schema-type :schema-required)
              :else x))]
    (walk/postwalk f form)))

(defmacro defmapping [name form]
  `(do
     (def ~name (schema-walk ~form))
     (def ~(symbol (str name "Raw")) ~form)
     (def ~(symbol (str name "Mapping")) (mapping-walk ~form))))

(def not-analyzed
  {:type :string
   :index "not_analyzed"
   :schema-type s/Str})

(def analyzed
  {:type :string
   :schema-type s/Str})

(def long
  {:type :long
   :schema-type s/Num})

(def double
  {:type :double
   :schema-type s/Num})

(def boolean
  {:type :boolean
   :schema-type s/Bool})

(def date
  {:type :date
   :schema-type s/Str})

(def multi-string
  {:type :string
   :schema-type s/Str
   ;; Doesn't need Schema, since it's invisible to Clojure
   :fields {:analyzed
            {:type :string
             :index :analyzed}}})

(def opt-not-analyzed
  (assoc not-analyzed :schema-required false))

(def opt-analyzed
  (assoc analyzed :schema-required false))

(def opt-multi-string
  (assoc multi-string :schema-required false))

(def opt-date
  (assoc date :schema-required false))

(def not-analyzed-string-list
  (assoc not-analyzed :schema-type [s/Str]))

