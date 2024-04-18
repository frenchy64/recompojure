(ns com.recompojure.compojure-api1
  "Exposes the API of compojure.api.core v1.1.13 but compiling to reitit."
  (:require [com.recompojure.compojure-api1.impl :as impl]
            [clojure.set :as set]))

(def ^:private options {})

(impl/load-api `options)

(defmacro undocumented
  "Routes without route-documentation. Can be used to wrap routes,
  not satisfying compojure.api.routes/Routing -protocol."
  [& handlers]
  (into ["" {:no-doc true}] handlers))
