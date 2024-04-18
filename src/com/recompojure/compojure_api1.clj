(ns com.recompojure.compojure-api1
  "Exposes the API of compojure.api.core v1.1.13 but compiling to reitit."
  (:require [com.recompojure.compojure-api1.impl :as impl]
            [clojure.set :as set]))

(def ^:private options {})

(impl/load-api `options)

;;TODO this isn't right
(defn routes
  "Create a Ring handler by combining several handlers into one."
  [& handlers]
  (vec handlers))

(defmacro undocumented
  "Routes without route-documentation. Can be used to wrap routes,
  not satisfying compojure.api.routes/Routing -protocol."
  [& handlers]
  (into ["" {:no-doc true}] handlers))

(defmacro middleware
  "Wraps routes with given middlewares using thread-first macro.
  Note that middlewares will be executed even if routes in body
  do not match the request uri. Be careful with middlewares that
  have side-effects."
  {:style/indent 1}
  [middleware & body-exprs]
  `["" {:middleware ~middleware}
    [~@body-exprs]])

(def ^:private allowed-context-options #{:tags :capabilities :description :responses :summary})

(defmacro context
  "Like compojure.api.core/context, except the binding vector must be empty and
  no binding-style options are allowed. This is to prevent the passed routes
  from being reinitialized on every request."
  {:style/indent 2}
  [path arg & args]
  (when-not (and (vector? arg)
                 (= [] arg))
    (throw (ex-info (str "Not allowed to bind anything in context, push into HTTP verbs instead: " (pr-str arg))
                    {})))
  (let [[options body-exprs] (impl/extract-parameters args true)
        _ (impl/check-return-banned! options)
        _ (when-some [extra-keys (not-empty (set/difference (set (keys options))
                                                            allowed-context-options))]
            (throw (ex-info (str "Not allowed these options in `context`, push into HTTP verbs instead: "
                                 (pr-str (sort extra-keys)))
                            {})))
        reitit-opts (let [{:keys [tags description summary capabilities responses]} options]
                      (cond-> {}
                        tags (assoc-in [:swagger :tags] (list 'quote tags))
                        description (assoc-in [:swagger :description] description)
                        summary (assoc-in [:swagger :summary] summary)
                        #_#_;;TODO
                        capabilities (update :middleware (fn [prev]
                                                           (assert (not prev))
                                                           [[`(wrap-capabilities-stub ~capabilities)]]))
                        responses (assoc :responses `(impl/compojure->reitit-responses ~responses))))]
    `[~path
      ~@(some-> (not-empty reitit-opts) list)
      (routes ~@body-exprs)]))
