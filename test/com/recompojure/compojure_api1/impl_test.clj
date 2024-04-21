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
  (is (= ':foo (impl/destructuring-key-for-nesting {} :foo)))
  (is (= ':foo (impl/destructuring-key-for-nesting 'a :foo)))
  (is (= ':foo (impl/destructuring-key-for-nesting '{::keys [a]} :foo)))
  (is (= ':foo (impl/destructuring-key-for-nesting '{{:keys [bar]} :bar} :foo)))
  (is (= 'a (impl/destructuring-key-for-nesting '{a :foo} :foo)))
  (is (= '{:keys [bar]} (impl/destructuring-key-for-nesting '{{:keys [bar]} :foo} :foo)))
  (is (thrown? AssertionError (impl/destructuring-key-for-nesting '{a :foo b :foo} :foo)))
  (is (thrown? AssertionError (impl/destructuring-key-for-nesting '[{a :foo b :foo}] :foo))))

(deftest visit-nested-map-destructuring-test
  (is (= '{b :a} (impl/visit-nested-map-destructuring nil :a (fn [_] 'b))))
  (is (= '{b :a} (impl/visit-nested-map-destructuring {} :a (fn [_] 'b))))
  (is (= '{b :a :as this} (impl/visit-nested-map-destructuring 'this :a (fn [_] 'b))))
  (is (= '{b :b} (impl/visit-nested-map-destructuring {:as :a} :b (fn [_] 'b))))
  (is (thrown? AssertionError (impl/visit-nested-map-destructuring '{foo :a} :b (fn [_] 'foo))))
  (is (thrown? AssertionError (impl/visit-nested-map-destructuring 'this {:as :a} (fn [_] 'b))))
  (is (= '{{:keys [b a]} :a} (impl/visit-nested-map-destructuring
                               '{{:keys [b]} :a}
                               :a
                               (fn [d] (update d :keys conj 'a))))))

(deftest add-as-destructuring-test
  (is (= 'a (impl/add-as-destructuring nil 'a)))
  (is (= 'a (impl/add-as-destructuring 'a 'a)))
  (is (thrown? AssertionError (impl/add-as-destructuring 'sym 'a)))
  (is (thrown? AssertionError (impl/add-as-destructuring '{:as sym} 'a)))
  (is (thrown? AssertionError (impl/add-as-destructuring '[:as sym] 'a)))
  (is (= '{:keys [b] :as a} (impl/add-as-destructuring '{:keys [b]} 'a)))
  (is (= '{:keys [b] :as a} (impl/add-as-destructuring '{:keys [b]} 'a))))

(deftest update-in-map-destructuring-test
  (is (= '{{:keys [b]} :a, :as c}
         (impl/update-in-map-destructuring
           '{{:keys [b]} :a}
           []
           impl/add-as-destructuring 'c)))
  (is (= '{{:keys [b] :as c} :a}
         (impl/update-in-map-destructuring
           '{{:keys [b]} :a}
           [:a]
           impl/add-as-destructuring 'c)))
  ;;TODO smarter: {{:keys [b]} :a}
  (is (= '{{b :b :keys [b]} :a}
         (impl/update-in-map-destructuring
           '{{:keys [b]} :a}
           [:a :b]
           impl/add-as-destructuring 'c))))

(deftest destructuring-ast-test
  (is (= {:op :placeholder} (impl/destructuring-ast nil)))
  (is (= {:op :local :name 'a} (impl/destructuring-ast 'a)))
  (is (= {:op :map} (impl/destructuring-ast '{})))
  (is (= '{:op :map, :nested {:nest {:op :local, :name nested}}} (impl/destructuring-ast '{nested :nest})))
  (is (= '{:op :map, :as e, :nested {:nest {:op :local, :name nested}}}
         (impl/destructuring-ast '{nested :nest :as e})))
  (is (= '{:op :map, :as e, :nested {:nest {:op :local, :name nested}}}
         (impl/destructuring-ast '{nested :nest :as e})))
  (is (= '{:op :map, :as e, :nested {:nest {:op :map, :as nested, :keys #{a}}}}
         (impl/destructuring-ast '{{:keys [a] :as nested} :nest :as e}))))

(deftest compile-ast-test
  (is (= '___# (binding [impl/*gensym* (comp symbol #(str % "__#"))]
                 (impl/compile-ast {:op :placeholder}))))
  (is (= 'default__#
         (binding [impl/*gensym* (comp symbol #(str % "__#"))]
           (impl/compile-ast {:op :placeholder :name "default"}))))
  (is (= 'a (impl/compile-ast {:op :local :name 'a})))
  (is (= {} (impl/compile-ast {:op :map})))
  (is (= '{nested :nest} (impl/compile-ast '{:op :map, :nested {:nest {:op :local, :name nested}}})))
  (is (= '{nested :nest :as e}
         (impl/compile-ast '{:op :map, :as e, :nested {:nest {:op :local, :name nested}}})))
  (is (= '{{:as nested, :keys [a]} :nest, :as e}
         (impl/compile-ast '{:op :map, :as e, :nested {:nest {:op :map, :as nested, :keys #{a}}}})))
  (is (= '{{:as nested, :keys [a]} :nest, :as e}
         (-> '{{:as nested, :keys [a]} :nest, :as e}
             impl/destructuring-ast
             impl/compile-ast)))
  (is (= '{{:as nested, :keys [a b c]} :nest, :as e}
         (-> '{{:as nested, :keys [b a c]} :nest, :as e}
             impl/destructuring-ast
             impl/compile-ast))))

(def canon #(-> % impl/destructuring-ast impl/canonicalize-destructuring impl/compile-ast))

(deftest canonicalize-destructuring-test
  (is (= 'a (canon 'a)))
  (is (= '{:keys [foo]} (canon '{foo :foo})))
  )

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
