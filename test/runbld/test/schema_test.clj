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
          {:id {:_schema {:type schema.core/Str
                          :required false}
                :_mapping {:type "string"
                           :index "not_analyzed"}}})))
  (is (= {(s/required-key :id) schema.core/Str}
         (schema-walk
          {:id {:_schema {:type schema.core/Str
                          :required true}
                :_mapping {:type "string"
                           :index "not_analyzed"}}})))
  (is (= {(s/required-key :id) schema.core/Str
          (s/optional-key :bar)
          {(s/optional-key :baz) (schema.core/maybe schema.core/Str)}}
         (schema-walk
          {:id {:_schema {:type schema.core/Str
                          :required true}
                :_mapping {:type "string"
                           :index "not_analyzed"}}
           :bar {:properties
                 {:baz
                  {:_schema {:type (schema.core/maybe
                                    schema.core/Str)
                             :required false}
                   :_mapping {:type "string"
                              :index "not_analyzed"}}}
                 :_schema {:required false}}}))))

(deftest t-mapping-walk
  (is (= {:id
          {:type "string"
           :index "not_analyzed"}}
         (mapping-walk
          {:id {:_schema {:type schema.core/Str
                          :required false}
                :_mapping {:type "string"
                           :index "not_analyzed"}}})))
  (is (= {:id {:type "string"
               :index "not_analyzed"}
          :bar {:properties
                {:baz {:type "string"
                       :index "not_analyzed"}
                 :quux {:type "long"}}}}
         (mapping-walk
          {:id {:_schema {:type schema.core/Str
                          :required true}
                :_mapping {:type "string"
                           :index "not_analyzed"}}
           :bar {:properties
                 {:baz
                  {:_schema {:type (schema.core/maybe
                                    schema.core/Str)
                             :required false}
                   :_mapping {:type "string"
                              :index "not_analyzed"}}
                  :quux
                  {:_schema {:type schema.core/Num
                             :required true}
                   :_mapping {:type "long"}}}
                 :_schema {:required false}}}))))
