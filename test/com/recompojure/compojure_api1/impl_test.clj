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

(defn add-d [d path nme default]
  (-> d
      impl/destructuring-ast
      impl/canonicalize-destructuring
      (impl/add-destructuring-for path nme default)
      impl/compile-ast))

(deftest add-destructuring-for-test
  (is (= 'foo (add-d nil [] 'foo nil)))
  (is (= '{:as foo} (add-d {} [] 'foo nil)))
  (is (= '{:as foo} (add-d {} [] 'foo nil)))
  (is (thrown? AssertionError (add-d 'a [] 'foo nil)))
  (is (= '{foo :a :as a} (add-d 'a [:a] 'foo nil)))
  (is (= '{:keys [a] :as a} (add-d 'a [:a] 'a nil)))
  (is (= '{{:keys [b]} :a} (add-d nil [:a :b] 'b nil)))
  (is (= '{{:keys [b]} :a :as a} (add-d 'a [:a :b] 'b nil)))
  )
