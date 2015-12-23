(ns runbld.schema.mapping
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

(defn has-nesting? [x]
  (and
   (map-entry? x)
   (:properties (entry-val x))))

(defn entry [k v]
  (first {k v}))

(defn schema-walk [form]
  (let [has-schema-entry? (fn [x]
                            (and
                             (map-entry? x)
                             (:_schema (entry-val x))))
        make-key (fn [x]
                   ((if (-> x entry-val :_schema :required)
                      s/required-key s/optional-key)
                    (entry-key x)))
        f (fn [x]
            (cond

              (has-nesting? x)
              (entry
               (make-key x) (schema-walk
                             (:properties
                              (entry-val x))))

              (has-schema-entry? x)
              (entry
               (make-key x)
               (-> x entry-val :_schema :type))
              :else x))]
    (walk/postwalk f form)))

(defn mapping-walk [form]
  (let [has-mapping-entry? (fn [x]
                            (and
                             (map-entry? x)
                             (:_mapping (entry-val x))))
        f (fn [x]
            (cond
              (has-mapping-entry? x)
              (entry (entry-key x) (:_mapping (entry-val x)))
              (map? x) (dissoc x :_schema)
              :else x))]
    (walk/postwalk f form)))

(defmacro defmapping [name map]
  `(do
     (def ~name ~(schema-walk map))
     (def ~(symbol (str name "Mapping")) ~(mapping-walk map))))

