;; TODO should :query-params be automatically optionalized? :query?
(ns com.recompojure.compojure-api1.impl
  "Exposes the API of compojure.api.core v1.1.13 but compiling to reitit."
  (:require [clojure.set :as set]
            [clojure.walk :as walk]
            [schema-tools.core :as st]))

(def ^:private ^:dynamic *gensym* gensym)

(defn options-sym [sym-or-options]
  {:post [(qualified-symbol? %)]}
  (if (symbol? sym-or-options)
    sym-or-options
    (::options-sym sym-or-options)))

(defn resolve-options [sym-or-options]
  (if (map? sym-or-options)
    sym-or-options
    (let [sym sym-or-options]
      (assert (qualified-symbol? sym))
      (let [v (ns-resolve sym)
            _ (assert (var? v))
            options @sym]
        (assert (map? options) (str "Missing options from " sym))
        (assoc options ::options-sym sym)))))

;compojure.api.common
(defn- plain-map?
  "checks whether input is a map, but not a record"
  [x] (and (map? x) (not (record? x))))

;compojure.api.common
(defn extract-parameters
  "Extract parameters from head of the list. Parameters can be:

  1. a map (if followed by any form) `[{:a 1 :b 2} :body]` => `{:a 1 :b 2}`
  2. list of keywords & values `[:a 1 :b 2 :body]` => `{:a 1 :b 2}`
  3. else => `{}`

  Returns a tuple with parameters and body without the parameters"
  [c expect-body]
  (cond
    (and (plain-map? (first c)) (or (not expect-body) (seq (rest c))))
    [(first c) (seq (rest c))]

    (keyword? (first c))
    (let [parameters (->> c
                          (partition 2)
                          (take-while (comp keyword? first))
                          (mapcat identity)
                          (apply array-map))
          form (drop (* 2 (count parameters)) c)]
      [parameters (seq form)])

    :else
    [{} (seq c)]))

;;TODO make ns extensible, use these as examples
(defn- ident->map-stub [& args] (assert nil (str "stub " `ident->map-stub)))
(defn- wrap-capabilities-stub [capabilities]
  (let [check-capabilities! (fn [{:keys [identity]}]
                              (assert nil "TODO")
                              #_
                              (require-capability!
                                capabilities
                                identity))]
    (fn [handler]
      (fn
        ([request]
         (check-capabilities! request)
         (handler request))
        ([request respond raise]
         (when (try (check-capabilities! request)
                    true
                    (catch Throwable e
                      (raise e)
                      false))
           (handler request respond raise)))))))

(defn check-return-banned! [options]
  (when-some [[_ schema] (find options :return)]
    (throw (ex-info (format (str ":return is banned, please use :responses instead.\n"
                                 "In this case, :return %s is equivalent to :responses {200 {:schema %s}}.\n"
                                 "For 204, you can use :responses {204 nil}.\n"
                                 "For catch-all, use :responses {:default {:schema SCHEMA}}")
                            schema schema)
                    {}))))


(def ^:private allowed-endpoint-options #{:responses :capabilities :auth-identity :identity-map :query-params :path-params
                                          :description :tags :no-doc :summary :produces :middleware :query :body})
(comment
  ;; todo list
  (set/difference @#'ctia.lib.compojure.api.core/allowed-endpoint-options
                  allowed-endpoint-options)
  
  )

(defn validate-responses! [responses]
  (assert (map? responses))
  (doseq [[k v] responses]
    (assert (nat-int? k) (pr-str k))
    (assert (<= 0 k 599) (pr-str k))
    (when (some? v)
      (assert (map? v) (pr-str v))
      (assert (:schema v) (pr-str v))
      (assert (-> v keys set (disj :schema :description) empty?) (pr-str v)))))

;; {200 {:schema Foo}} => {200 {:body Foo}}
(defn compojure->reitit-responses [responses]
  (validate-responses! responses)
  (update-vals responses (fn [rhs]
                           (when (some? rhs)
                             (assert (map? rhs))
                             (let [unknown-keys (-> rhs keys set (disj :schema))]
                               (assert (empty? unknown-keys) unknown-keys))
                             (assert (:schema rhs))
                             (set/rename-keys rhs {:schema :body})))))

(defn ^:private parse-params [params]
  (assert (vector? params) (str "params must be a vector: " (pr-str params)))
  (loop [params params
         result (sorted-map)]
    (if (empty? params)
      result
      (let [f (first params)]
        (if (map? f)
          ;; [{wait_for :- (describe s/Bool "wait for patched entity to be available for search") nil}]
          ;; =>
          ;; {wait_for {:schema (describe s/Bool "wait for patched entity to be available for search")
          ;;            :default nil}}
          (let [m f
                _ (assert (= 2 (count m)) (str "incorrect map params syntax, must be length 2: " (pr-str m)))
                [[sym _] [schema default]]
                (let [[l r] (seq m)]
                  (if (-> l val (= :-))
                    [l r]
                    [r l]))]
            (recur (next params)
                   (assoc result sym {:schema schema
                                      :default default})))
          ;; [wait_for :- (describe s/Bool "wait for patched entity to be available for search")]
          ;; =>
          ;; {wait_for {:schema (describe s/Bool "wait for patched entity to be available for search")}}
          (let [[turnstile schema & params] (next params)
                sym f]
            (assert (simple-symbol? sym) (str "expected first value to be a simple symbol in " [sym turnstile schema]))
            (assert (= :- turnstile) (str "expected :- after " sym))
            (assert schema (str "missing schema in params for: " sym))
            (recur params
                   (assoc result sym {:schema schema}))))))))

;; idea: we could support a lot more `context` restructure middleware if we asserted that
;; bindings created from a `context` could only be used in route bodies.
;; OK
;;   (context "" req
;;     (GET "" [] req))
;;   (context "" []
;;     :body [body Foo]
;;     (GET "" [] foo))
;; banned
;;   (context "" req
;;     :middleware [req]
;;     (GET "" [] req))
;;   (context "" req
;;     :body [req req]
;;     (GET "" [] req))
(defn ^:private prevent-scoping-difference-error!
  [arg options]
  (walk/postwalk (fn [s]
                   (if (= arg s)
                     (throw (ex-info (format (str "There is a key difference in scoping between compojure-api and "
                                                  "our compilation to reitit. The request has been bound to %s "
                                                  "but this symbol occurs in the restructuring options. "
                                                  "The request is not in scope here in reitit, so "
                                                  "please rename %s so this incomplete analysis can rule out this "
                                                  "mistake.")
                                             arg arg)
                                     {}))
                     s))
                 options)
  nil)

(def ^:private allowed-context-options #{:tags :capabilities :description :responses :summary})

(defn context
  "Like compojure.api.core/context, except the binding vector must be empty and
  no binding-style options are allowed. This is to prevent the passed routes
  from being reinitialized on every request."
  [OPTIONS path arg args]
  (when-not (and (vector? arg)
                 (= [] arg))
    (throw (ex-info (str "Not allowed to bind anything in context, push into HTTP verbs instead: " (pr-str arg))
                    {})))
  (let [[options body-exprs] (extract-parameters args true)
        _ (check-return-banned! options)
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
                        responses (assoc :responses `(compojure->reitit-responses ~responses))))]
    `[~path
      ~@(some-> (not-empty reitit-opts) list)
      (routes '~(options-sym OPTIONS) ~(vec body-exprs))]))

(defn ^:private restructure-endpoint [http-kw [path arg & args] options]
  (assert (simple-keyword? http-kw))
  (assert (or (= [] arg)
              (simple-symbol? arg))
          (pr-str arg))
  (let [[{:keys [capabilities auth-identity identity-map tags middleware] :as options} body-exprs] (extract-parameters args true)
        _ (check-return-banned! options)
        _ (when (simple-symbol? arg)
            (prevent-scoping-difference-error! arg options))
        _ (when-some [extra-keys (not-empty (set/difference (set (keys options))
                                                            allowed-endpoint-options))]
            (throw (ex-info (str "Not allowed these options in endpoints: "
                                 (pr-str (sort extra-keys)))
                            {})))
        responses (when-some [[_ responses] (find options :responses)]
                    `(compojure->reitit-responses ~responses))
        query-params (when-some [[_ query-params] (find options :query-params)]
                       (parse-params query-params))
        query (when-some [[_ [bind schema :as query]] (find options :query)]
                (when query-params
                  (throw (ex-info "Cannot use both :query-params and :query, please combine them."
                                  {})))
                (when-not (and (vector? query) (= 2 (count query)))
                  (throw (ex-info ":query must be a vector of length 2" {})))
                (assert bind)
                (assert schema)
                {:bind bind
                 :schema schema})
        body (when-some [[_ [bind schema :as body]] (find options :body)]
               (when-not (and (vector? body) (= 2 (count body)))
                 (throw (ex-info (str ":body must be a vector of length 2: " (pr-str body)) {})))
               (assert bind)
               (assert schema)
               {:bind bind
                :schema schema})
        path-params (when-some [[_ path-params] (find options :path-params)]
                      (parse-params path-params))
        greq (*gensym* "req")
        gparameters (delay (*gensym* "parameters"))
        gidentity (delay (*gensym* "identity"))
        needs-parameters? (or query-params path-params query body)
        ;; `gs` are uncapturable variables via gensym. they are bound first so
        ;; they can be bound to capturable expressions.
        ;; `scoped` are capturable variables provided by user. they are bound last,
        ;; and they are bound to uncapturable expressions.
        {:keys [scoped gs]
         :or {gs [] scoped []}} (merge-with
                                  into
                                  (when (simple-symbol? arg)
                                    {:scoped [arg greq]})
                                  (when needs-parameters?
                                    {:gs [@gparameters (list :parameters greq)]})
                                  (when query-params
                                    (let [gquery (*gensym* "query")]
                                      (apply merge-with into 
                                             {:gs [gquery (list :query @gparameters)]}
                                             (map (fn [[sym {:keys [default]}]]
                                                    (let [gdefault (*gensym* (str (name sym) "-default"))]
                                                      {:gs [gdefault default]
                                                       :scoped [sym (list `get gquery (keyword sym) gdefault)]}))
                                                  query-params))))
                                  (when path-params
                                    (let [gpath (*gensym* "path")]
                                      (apply merge-with into 
                                             {:gs [gpath (list :path @gparameters)]}
                                             (map (fn [[sym {:keys [schema] :as opts}]]
                                                    (assert (= [:schema] (keys opts)) "no default allowed for path params")
                                                    {:scoped [sym (list `get gpath (keyword sym))]})
                                                  path-params))))
                                  (when-some [{:keys [bind]} query]
                                    (let [gquery (*gensym* "query")]
                                      {:gs [gquery (list :query @gparameters)]
                                       :scoped [bind gquery]}))
                                  (when-some [{:keys [bind]} body]
                                    (let [gbody (*gensym* "body")]
                                      {:gs [gbody (list :body @gparameters)]
                                       :scoped [bind gbody]}))
                                  (when (or auth-identity identity-map)
                                    {:gs [@gidentity (list :identity greq)]})
                                  (when auth-identity
                                    (assert (simple-symbol? auth-identity) (str ":auth-identity must be a simple symbol: "
                                                                                (pr-str auth-identity)))
                                    {:scoped [auth-identity @gidentity]})
                                  (when identity-map
                                    (assert (simple-symbol? identity-map) (str ":identity-map must be a simple symbol: "
                                                                               (pr-str identity-map)))
                                    {:scoped [identity-map (list `ident->map-stub @gidentity)]}))
        _ (when (seq gs)
            (assert (apply distinct? (map first (partition 2 gs)))))
        _ (when (seq scoped)
            (let [names (map first (partition 2 scoped))]
              ;; we can lift this once we ensure we parse options deterministically. i.e., that `options` is
              ;; in the same order as provided by the user.
              (assert (apply distinct? names)
                      (str "ERROR: cannot shadow variables in endpoints, please rename to avoid clashes: "
                           (pr-str (sort names))))))]
    [path {http-kw (cond-> {:handler `(fn [~greq]
                                        (let ~(into gs scoped)
                                          (do ~@body-exprs)))}
                     (contains? options :description) (assoc-in [:swagger :description] (:description options))
                     ;; literal in compojure-api, so we conserve the semantics
                     tags (assoc-in [:swagger :tags] (list 'quote tags))
                     (contains? options :no-doc) (assoc-in [:swagger :no-doc] (:no-doc options))
                     (contains? options :summary) (assoc-in [:swagger :summary] (:summary options))
                     (contains? options :produces) (assoc-in [:swagger :produces] (:produces options))
                     responses (assoc :responses responses)
                     ;; made :middleware an expression, not sure what compojure-api does here.
                     middleware (update :middleware (fn [prev]
                                                      (assert (not prev))
                                                      middleware))
                     #_#_;;TODO
                     capabilities (update :middleware (fn [prev]
                                                        (let [this-middleware [`(wrap-capabilities-stub ~capabilities)]]
                                                          ;; how to combine with existing :middleware? just ask the user do it.
                                                          (when prev
                                                            (throw (ex-info (format
                                                                              (str "Combining :middleware and :capabilities not yet supported. "
                                                                                   "Please use :middleware %s instead of :capabilities %s.\n"
                                                                                   "The complete middleware might look like: :middleware (conj %s %s).")
                                                                              (pr-str this-middleware) (pr-str capabilities)
                                                                              (pr-str prev) (pr-str (first this-middleware)))
                                                                            {})))
                                                          [this-middleware])))
                     query-params (assoc-in [:parameters :query]
                                            ;; TODO does compojure-api optionalize?
                                            (list `st/optional-keys
                                                  (into {} (map (fn [[sym {:keys [schema]}]]
                                                                  {(keyword sym) schema}))
                                                        query-params)))
                     query (update-in [:parameters :query] (fn [prev]
                                                             (assert (not prev))
                                                             ;; TODO does compojure-api optionalize?
                                                             (:schema query)))
                     body (update-in [:parameters :body] (fn [prev]
                                                           (assert (not prev))
                                                           (:schema body)))
                     path-params (assoc-in [:parameters :path]
                                           (into {} (map (fn [[sym {:keys [schema]}]]
                                                           {(keyword sym) schema}))
                                                 path-params)))}]))

(defn GET    [options args] (restructure-endpoint :get args options))
(defn ANY    [options args] (restructure-endpoint :any args options))
(defn PATCH  [options args] (restructure-endpoint :patch args options))
(defn DELETE [options args] (restructure-endpoint :delete args options))
(defn POST   [options args] (restructure-endpoint :post args options))
(defn PUT    [options args] (restructure-endpoint :put args options))

(defn- quoted [l]
  (when (and (seq? l)
             (= 2 (count l))
             (= 'quote (first l)))
    (second l)))

;;TODO this isn't right
(defn routes
  "Create a Ring handler by combining several handlers into one."
  [options handlers]
  (vec handlers))

(defn middleware
  "Wraps routes with given middlewares using thread-first macro.
  Note that middlewares will be executed even if routes in body
  do not match the request uri. Be careful with middlewares that
  have side-effects."
  [options middleware body-exprs]
  `["" {:middleware ~middleware}
    [~@body-exprs]])

(defmacro load-api [options]
  (assert (and (seq? options)
               (= 2 (count options))
               (= 'quote (first options))
               (qualified-symbol? (second options)))
          "Options must be a quoted qualified symbol whose var contains your configuration, like: (load-api `options)")
  `(let [options# ~options]
     (defmacro ~'GET     {:style/indent 2 :arglists '([& ~'args])} [& args#] (GET options# args#))
     (defmacro ~'ANY     {:style/indent 2 :arglists '([& ~'args])} [& args#] (ANY options# args#))
     (defmacro ~'PATCH   {:style/indent 2 :arglists '([& ~'args])} [& args#] (PATCH options# args#))
     (defmacro ~'DELETE  {:style/indent 2 :arglists '([& ~'args])} [& args#] (DELETE options# args#))
     (defmacro ~'POST    {:style/indent 2 :arglists '([& ~'args])} [& args#] (POST options# args#))
     (defmacro ~'PUT     {:style/indent 2 :arglists '([& ~'args])} [& args#] (PUT options# args#))
     (defmacro ~'context
       "Like compojure.api.core/context, except the binding vector must be empty and
       no binding-style options are allowed. This is to prevent the passed routes
       from being reinitialized on every request."
       {:style/indent 2 :arglists '~'([path arg & args])}
       [path# arg# & args#]
       (context options# path# arg# args#))
     (defn ~'routes
       "Create a Ring handler by combining several handlers into one."
       {:style/indent 2 :arglists '~'([& handlers])}
       [& handlers#]
       (routes options# handlers#))
     (defmacro ~'middleware
       "Wraps routes with given middlewares using thread-first macro.
       Note that middlewares will be executed even if routes in body
       do not match the request uri. Be careful with middlewares that
       have side-effects."
       {:style/indent 1}
       [middleware# & body-exprs#]
       (middleware options# middleware# body-exprs#))
     ))
