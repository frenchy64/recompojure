(ns com.recompojure.compojure-api1-extension-test
  (:require [com.recompojure.compojure-api1.impl :as impl]
            [clojure.set :as set]))

(def ^:private options {:impl :compojure-api1
                        :extra-allowed-endpoint-options #{:capabilities :auth-identity :identity-map}})

(impl/load-api `options)
