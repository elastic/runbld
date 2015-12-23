(ns runbld.test.schema-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [schema.core :as s])
  (:require [clj-time.core :as t]
            [runbld.schema.mapping :refer [schema-walk mapping-walk]]
            :reload-all))

(use-fixtures :once schema.test/validate-schemas)

(deftest t-schema-walk
  (is (= {(s/optional-key :id) schema.core/Str}
         (schema-walk
          {:id {:type "string"
                :index "not_analyzed"
                :schema-type schema.core/Str
                :schema-required false}})))
  (is (= {(s/required-key :id) schema.core/Str}
         (schema-walk
          {:id {:type "string"
                :index "not_analyzed"
                :schema-type schema.core/Str
                :schema-required true}})))
  (is (= {(s/required-key :id) schema.core/Str
          (s/optional-key :bar)
          {(s/optional-key :baz) (schema.core/maybe schema.core/Str)
           (s/required-key :quux) {(s/required-key :quux1) schema.core/Num}}}
         (schema-walk
          {:id {:type "string"
                :index "not_analyzed"
                :schema-type schema.core/Str}
           :bar {:properties
                 {:baz
                  {:type "string"
                   :index "not_analyzed"
                   :schema-type (schema.core/maybe
                                 schema.core/Str)
                   :schema-required false}
                  :quux
                  {:properties
                   {:quux1
                    {:type "long"
                     :index "not_analyzed"
                     :schema-type schema.core/Num}}}}
                 :schema-required false}}))))

(deftest t-mapping-walk
  (is (= {:id
          {:type "string"
           :index "not_analyzed"}}
         (mapping-walk
          {:id {:type "string"
                :index "not_analyzed"
                :schema-type schema.core/Str
                :schema-required false}})))
  (is (= {:id {:type "string"
               :index "not_analyzed"}
          :bar {:properties
                {:baz {:type "string"
                       :index "not_analyzed"}
                 :quux
                 {:properties
                  {:quux1 {:type "long", :index "not_analyzed"}}}}}}
         (mapping-walk
          {:id {:type "string"
                :index "not_analyzed"
                :schema-type schema.core/Str}
           :bar {:properties
                 {:baz
                  {:type "string"
                   :index "not_analyzed"
                   :schema-type (schema.core/maybe
                                 schema.core/Str)
                   :schema-required false}
                  :quux
                  {:properties
                   {:quux1
                    {:type "long"
                     :index "not_analyzed"
                     :schema-type schema.core/Num}}}}
                 :schema-required false}}))))
