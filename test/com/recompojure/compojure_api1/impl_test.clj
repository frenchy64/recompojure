(ns com.recompojure.compojure-api1.impl-test
  (:require [clojure.test :refer [is]]
            [com.recompojure.compojure-api1.impl :as impl]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [testing deftest]]
            [reitit.coercion.schema :as rcs]
            [reitit.ring :as ring]
            [reitit.ring.coercion :as rrc]
            [reitit.ring.middleware.parameters :as parameters]
            [ring.swagger.json-schema :refer [describe]]
            [schema-tools.core :as st]
            [schema.core :as s]
            [schema.utils :as su]))

(deftest destructuring-key-for-nesting-test
  (is (= ':foo (impl/destructuring-key-for-nesting nil :foo)))
  (is (= ':foo (impl/destructuring-key-for-nesting {} :foo)))
  (is (= ':foo (impl/destructuring-key-for-nesting '{{:keys [bar]} :bar} :foo)))
  (is (= 'a (impl/destructuring-key-for-nesting '{a :foo} :foo)))
  (is (= '{:keys [bar]} (impl/destructuring-key-for-nesting '{{:keys [bar]} :foo} :foo)))
  (is (thrown? AssertionError (impl/destructuring-key-for-nesting '{a :foo b :foo} :foo))))

(deftest bindings-tree-test
  (is (= '{}
         (impl/bindings-tree
           []
           nil)))
  (is (= '{:get-in-request {:syms {req {:default nil, :path []}}}}
         (impl/bindings-tree
           [{:name 'req
             :op :get-in-request
             :path []}]
           nil)))
  (is (= '{:get-in-request
           {:children
            {:parameters
             {:children
              {:query
               {:children
                {:foo
                 {:syms {foo {:default nil, :path [:parameters :query :foo]}}}}}}}}}}
         (impl/bindings-tree
           [{:name 'foo
             :op :get-in-request
             :path [:parameters :query :foo]}]
           nil)))
  )
