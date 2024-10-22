(ns com.recompojure.compojure-api1-test
  (:require [clojure.test :refer [is]]
            [com.recompojure.compojure-api1 :as sut]
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

#_
(defmacro with-deterministic-gensym [& body]
  `(with-bindings {#'impl/*gensym* (let [a# (atom -1)]
                                     (fn [s#]
                                       {:pre [(string? s#)]
                                        :post [(symbol? ~'%)]}
                                       (symbol (str s# "__" (swap! a# inc')))))}
     (do ~@body)))

(defmacro with-no-repeat-gensym [& body]
  `(with-bindings {#'impl/*gensym* (let [seen# (atom #{})]
                                     (fn [s#]
                                       {:pre [(string? s#)]
                                        :post [(symbol? ~'%)]}
                                       #_
                                       (when (contains? (first (swap-vals! seen# conj s#)) s#)
                                         (throw (ex-info (str "Reused " s#) {})))
                                       (symbol (str s# "__#"))))}
     (do ~@body)))

(defn dexpand-1 [form]
  (with-no-repeat-gensym #_with-deterministic-gensym
    (macroexpand-1 form)))

;; adapted from clojure.repl/root-cause, but unwraps compiler exceptions
(defn root-cause [t]
  (loop [cause t]
    (if-let [cause (.getCause cause)]
      (recur cause)
      cause)))

(defn is-banned-macro [form msg]
  (try (dexpand-1 form)
       (is (and "expected-banned-error" false) (pr-str form))
       (catch Exception e
         (is (= msg (ex-message (root-cause e))) (pr-str form)))))

(def compojure->reitit-endpoints
  `{sut/GET :get
    sut/ANY :any
    sut/PATCH :patch
    sut/DELETE :delete
    sut/POST :post
    sut/PUT :put})

(deftest routes-test
  (is (= [["/blah" identity] ["/foo" identity]]
         (sut/routes ["/blah" identity] ["/foo" identity]))))

(deftest middleware-macro-test
  (is (= ["" {:middleware [[:some 1] [:middleware 2]]}
          [["/blah" identity] ["/foo" identity]]]
         (sut/middleware [[:some 1] [:middleware 2]]
           ["/blah" identity]
           ["/foo" identity])))
  (testing "200 response"
    (let [g (gensym)
          called (atom {:outer 0 :inner 0})]
      (is (= {:status 200 :body g}
             (let [mid (fn [handler]
                         (swap! called update :outer inc)
                         (fn
                           ([request]
                            (swap! called update :inner inc)
                            (handler
                              (assoc request ::middleware-called g)))
                           ([request respond raise]
                            (swap! called update :inner inc)
                            (handler
                              (assoc request ::middleware-called g)
                              respond
                              raise))))
                   app (ring/ring-handler
                         (ring/router
                           (sut/middleware
                             [mid]
                             (sut/GET
                               "/my-route" req
                               {:status 200
                                :body (::middleware-called req)}))))]
               (app {:request-method :get
                     :uri "/my-route"}))))
      ;; TODO why is :outer called twice?
      (is (= @called {:outer 2 :inner 1}))))
  )

(deftest middleware-restructure-test
  (testing "context"
    ;; could easily be supported if needed
    (is-banned-macro
      `(sut/context
         "/my-route" []
         :middleware [[~'render-resource-file]]
         ~'routes)
      "Not allowed these options in `context`, push into HTTP verbs instead: (:middleware)"))
  (testing "GET"
    (testing "expansion"
      #_;;TODO
      (testing "combining with :capabilities is banned"
        (is-banned-macro
          `(sut/GET
             "/my-route" []
             :capabilities ~'capabilities
             :middleware [[~'render-resource-file]]
             ~'routes)
          "Combining :middleware and :capabilities not yet supported. Please use :middleware [(com.recompojure.compojure-api1/wrap-capabilities-stub capabilities)] instead of :capabilities capabilities.\nThe complete middleware might look like: :middleware (conj [[render-resource-file]] (com.recompojure.compojure-api1/wrap-capabilities-stub capabilities))."))
      (is (= '["/my-route" {:get {:handler (clojure.core/fn [req__#] identity)
                                  :middleware [[render-resource-file]]}}]
             (dexpand-1
               `(sut/GET
                  "/my-route" []
                  :middleware [[~'render-resource-file]]
                  ~'identity)))))
    (testing "200 response"
      (let [g (gensym)
            called (atom {:outer 0 :inner 0})]
        (is (= {:status 200 :body g}
               (let [mid (fn [handler]
                           (swap! called update :outer inc)
                           (fn
                             ([request]
                              (swap! called update :inner inc)
                              (handler
                                (assoc request ::middleware-called g)))
                             ([request respond raise]
                              (swap! called update :inner inc)
                              (handler
                                (assoc request ::middleware-called g)
                                respond
                                raise))))
                     app (ring/ring-handler
                           (ring/router
                             (sut/GET
                               "/my-route" req
                               :middleware [mid]
                               {:status 200
                                :body (::middleware-called req)})))]
                 (app {:request-method :get
                       :uri "/my-route"}))))
        (is (= @called {:outer 1 :inner 1}))))))

;;FIXME runtime routing tests !!!!!!!!
(deftest context-test
  (is (= ["/my-route" [identity]]
         (sut/context
           "/my-route" []
           identity)))
  
  #_ ;;TODO
  (is (= '["/my-route" {:middleware [[(com.recompojure.compojure-api1/wrap-capabilities-stub :create-incident)]]}
           (com.recompojure.compojure-api1/routes clojure.core/identity)]
         (dexpand-1
           `(sut/context
              "/my-route" []
              :capabilities :create-incident
              identity))))
  #_ ;;TODO
  (is (= '["/my-route" {:middleware [[(com.recompojure.compojure-api1/wrap-capabilities-stub capabilities-are-expressions)]]}
           (com.recompojure.compojure-api1/routes clojure.core/identity)]
         (dexpand-1
           `(sut/context
              "/my-route" []
              :capabilities ~'capabilities-are-expressions
              identity))))
  
  
  (is (= ["/my-route"
          {:responses {200 {:body {:a (s/enum "schema")}}}}
          [identity]]
         (sut/context
           "/my-route" []
           :responses {200 {:schema {:a (s/enum "schema")}}}
           identity)))
  (is (= ["/my-route"
          {:responses {200 {:body {:a (s/enum "schema")}}}}
          [identity]]
         (let [responses-are-expressions {200 {:schema {:a (s/enum "schema")}}}]
           (sut/context
             "/my-route" []
             :responses responses-are-expressions
             identity)))))

(deftest get-test
  (is (= '["/my-route" {:get {:handler (clojure.core/fn [req__#] {:status 200})}}]
         (dexpand-1
           `(sut/GET "/my-route" []
                     {:status 200}))))
  (is (= {:status 200
          :body "here"}
         (let [app (ring/ring-handler
                     (ring/router
                       (sut/GET "/my-route" []
                                {:status 200
                                 :body "here"})))]
           (app {:request-method :get
                 :uri "/my-route"})))))

(deftest responses-test
  (testing "GET"
    (is (= '["/my-route" {:get {:handler (clojure.core/fn [req__#] {:status 200, :body 1})
                                :responses (com.recompojure.compojure-api1.impl/compojure->reitit-responses {200 {:schema schema.core/Int}})}}]
           (dexpand-1
             `(sut/GET "/my-route" []
                       :responses {200 {:schema s/Int}}
                       {:status 200
                        :body 1}))))
    (is (= {:status 200
            :body 1}
           (let [app (ring/ring-handler
                       (ring/router
                         (sut/GET "/my-route" []
                                  :responses {200 {:schema s/Int}}
                                  {:status 200
                                   :body 1})
                         {:data {:middleware [reitit.ring.coercion/coerce-response-middleware]
                                 :coercion reitit.coercion.schema/coercion}}))]
             (app {:request-method :get
                   :uri "/my-route"}))))
    (is (thrown? Exception "Response coercion failed"
                 (let [app (ring/ring-handler
                             (ring/router
                               (sut/GET "/my-route" []
                                        :responses {200 {:schema s/Bool}}
                                        {:status 200
                                         :body 1})
                               {:data {:middleware [reitit.ring.coercion/coerce-response-middleware]
                                       :coercion reitit.coercion.schema/coercion}}))]
                   (app {:request-method :get
                         :uri "/my-route"})))))
  (testing "context"
    (is (= '["/context" {:responses (com.recompojure.compojure-api1.impl/compojure->reitit-responses {200 {:schema schema.core/Int}})}
             (com.recompojure.compojure-api1.impl/routes
               (quote com.recompojure.compojure-api1/options)
               [(com.recompojure.compojure-api1/GET "/my-route" [] {:status 200, :body 1})])]
           (dexpand-1
             `(sut/context "/context" []
                           :responses {200 {:schema s/Int}}
                           (sut/GET "/my-route" []
                                    {:status 200
                                     :body 1})))))
    (is (= {:status 200
            :body 1}
           (let [app (ring/ring-handler
                       (ring/router
                         (sut/context "/context" []
                                      :responses {200 {:schema s/Int}}
                                      (sut/GET "/my-route" []
                                               {:status 200
                                                :body 1}))
                         {:data {:middleware [reitit.ring.coercion/coerce-response-middleware]
                                 :coercion reitit.coercion.schema/coercion}}))]
             (app {:request-method :get
                   :uri "/context/my-route"}))))
    (is (thrown? Exception "Response coercion failed"
                 (let [app (ring/ring-handler
                             (ring/router
                               (sut/context "/context" []
                                            :responses {200 {:schema s/Bool}}
                                            (sut/GET "/my-route" []
                                                     {:status 200
                                                      :body 1}))
                               {:data {:middleware [reitit.ring.coercion/coerce-response-middleware]
                                       :coercion reitit.coercion.schema/coercion}}))]
                   (app {:request-method :get
                         :uri "/context/my-route"}))))))

#_;;TODO
(deftest capabilities-test
  (testing "expansion"
    (is (= '["/my-route" {:get {:handler (clojure.core/fn [req__#] (clojure.core/let [] (do {:status 200, :body 1})))
                                :middleware [[(com.recompojure.compojure-api1/wrap-capabilities-stub :create-incident)]]}}]
           (dexpand-1
             `(sut/GET "/my-route" []
                       :capabilities :create-incident
                       {:status 200
                        :body 1})))))

  (testing "GET"
    (testing "401 on no identity"
      (is (thrown-with-msg? Exception #"HTTP 401"
                            (let [app (ring/ring-handler
                                        (ring/router
                                          (sut/GET "/my-route" []
                                                   :capabilities :create-incident
                                                   {:status 200
                                                    :body 1})))]
                              (app {:request-method :get
                                    :uri "/my-route"})))))
    (testing "403 on bad capability"
      (is (thrown-with-msg? Exception #"HTTP 403"
                            (let [app (ring/ring-handler
                                        (ring/router
                                          (sut/GET "/my-route" []
                                                   :capabilities :create-incident
                                                   {:status 200
                                                    :body 1})))]
                              (app {:request-method :get
                                    :uri "/my-route"
                                    :identity (->ReadOnlyIdentity)})))))
    (testing "200 on good capability"
      (is (= {:status 200
              :body 1}
             (let [app (ring/ring-handler
                         (ring/router
                           (sut/GET "/my-route" []
                                    :capabilities :create-incident
                                    {:status 200
                                     :body 1})))]
               (app {:request-method :get
                     :uri "/my-route"
                     :identity (->WriteIdentity 'name 'group)}))))))
  (testing "context"
    (is (thrown-with-msg? Exception #"HTTP 401"
                          (let [app (ring/ring-handler
                                      (ring/router
                                        (sut/GET "/my-route" []
                                                 :capabilities :create-incident
                                                 {:status 200
                                                  :body 1})))]
                            (app {:request-method :get
                                  :uri "/my-route"}))))
    (testing "401 on no identity"
      (is (thrown-with-msg? Exception #"HTTP 401"
                            (let [app (ring/ring-handler
                                        (ring/router
                                          (sut/context "/foo" []
                                                       :capabilities :create-incident
                                                       (sut/GET "/my-route" []
                                                                {:status 200
                                                                 :body 1}))))]
                              (app {:request-method :get
                                    :uri "/foo/my-route"})))))
    (testing "403 on bad capability"
      (is (thrown-with-msg? Exception #"HTTP 403"
                            (let [app (ring/ring-handler
                                        (ring/router
                                          (sut/context "/foo" []
                                                       :capabilities :create-incident
                                                       (sut/GET "/my-route" []
                                                                {:status 200
                                                                 :body 1}))))]
                              (app {:request-method :get
                                    :uri "/foo/my-route"
                                    :identity (->ReadOnlyIdentity)})))))
    (testing "200 on good capability"
      (is (= {:status 200
              :body 1}
             (let [app (ring/ring-handler
                         (ring/router
                           (sut/context "/foo" []
                                        :capabilities :create-incident
                                        (sut/GET "/my-route" []
                                                 {:status 200
                                                  :body 1}))))]
               (app {:request-method :get
                     :uri "/foo/my-route"
                     :identity (->WriteIdentity 'name 'group)})))))))

(deftest path-params-test
  (testing "context"
    (is-banned-macro
      `(sut/context
         "/my-route" []
         :path-params [~'id :- s/Str]
         ~'routes)
      "Not allowed these options in `context`, push into HTTP verbs instead: (:path-params)"))
  (testing "endpoints"
    (testing "expansion"
      (is (= '["/my-route" {:get {:handler (clojure.core/fn [{{{:keys [id]} :path} :parameters}]
                                             identity)
                                  :parameters {:path {:id schema.core/Str}}}}]
             (macroexpand-1
               `(sut/GET
                  "/my-route" []
                  :path-params [~'id :- s/Str]
                  ~'identity)))))
    (testing "200 response"
      (testing "passes schema"
        (let [g (str (random-uuid))]
          (is (= {:status 200
                  :body g}
                 (let [app (ring/ring-handler
                             (ring/router
                               (sut/GET
                                 "/:id/foo" []
                                 :path-params [id :- s/Str]
                                 {:status 200
                                  :body id})
                               {:data {:middleware [parameters/parameters-middleware
                                                    rrc/coerce-request-middleware]
                                       :coercion reitit.coercion.schema/coercion}}))]
                   (app {:request-method :get
                         :uri (str "/" g "/foo")})))))
        (testing "in context"
          (let [g (str (random-uuid))]
            (is (= {:status 200
                    :body g}
                   (let [app (ring/ring-handler
                               (ring/router
                                 (sut/context
                                   "/:id" []
                                   (sut/GET
                                     "/foo" []
                                     :path-params [id :- s/Str]
                                     {:status 200
                                      :body id}))
                                 {:data {:middleware [parameters/parameters-middleware
                                                      rrc/coerce-request-middleware]
                                         :coercion reitit.coercion.schema/coercion}}))]
                     (app {:request-method :get
                           :uri (str "/" g "/foo")})))))))
      (testing "fails schema"
        (try (let [app (ring/ring-handler
                         (ring/router
                           (sut/GET
                             "/:id/foo" []
                             :path-params [id :- s/Int]
                             {:status 200
                              :body id})
                           {:data {:middleware [parameters/parameters-middleware
                                                rrc/coerce-request-middleware]
                                   :coercion reitit.coercion.schema/coercion}}))]
               (app {:request-method :get
                     :uri "/not-a-number/foo"}))
             (is false)
             (catch Exception e
               (let [actual (-> (ex-data e)
                                (select-keys [:errors :type])
                                (update :errors update-vals su/validation-error-explain))]
                 (is (= {:errors {:id '(not (integer? "not-a-number"))}
                         :type :reitit.coercion/request-coercion}
                        actual)))))
        (testing "in context"
          (try (let [app (ring/ring-handler
                           (ring/router
                             (sut/context
                               "/:id" []
                               (sut/GET
                                 "/foo" []
                                 :path-params [id :- s/Int]
                                 {:status 200
                                  :body id}))
                             {:data {:middleware [parameters/parameters-middleware
                                                  rrc/coerce-request-middleware]
                                     :coercion reitit.coercion.schema/coercion}}))]
                 (app {:request-method :get
                       :uri "/not-a-number/foo"}))
               (is false)
               (catch Exception e
                 (let [actual (-> (ex-data e)
                                  (select-keys [:errors :type])
                                  (update :errors update-vals su/validation-error-explain))]
                   (is (= {:errors {:id '(not (integer? "not-a-number"))}
                           :type :reitit.coercion/request-coercion}
                          actual))))))))))

(deftest query-params-test
  (testing "context"
    (is-banned-macro
      `(sut/context
         "/my-route" []
         :query-params [{~'wait_for :- (describe s/Bool "wait for patched entity to be available for search") nil}]
         ~'routes)
      "Not allowed these options in `context`, push into HTTP verbs instead: (:query-params)"))
  (testing "GET"
    (testing "expansion"
      (is (= '["/my-route" {:get {:handler (clojure.core/fn [req__#]
                                             (clojure.core/let [parameters__# (:parameters req__#)
                                                                query__# (:query parameters__#)
                                                                wait_for-default__# default
                                                                wait_for (clojure.core/get query__# :wait_for wait_for-default__#)]
                                               (do clojure.core/identity)))
                                  :parameters {:query (schema-tools.core/optional-keys
                                                        {:wait_for (ring.swagger.json-schema/describe
                                                                     schema.core/Bool
                                                                     "wait for patched entity to be available for search")})}}}]
             (dexpand-1
               `(sut/GET
                  "/my-route" []
                  :query-params [{~'wait_for :- (describe s/Bool "wait for patched entity to be available for search") ~'default}]
                  identity)))))
    (testing "200 response"
      (doseq [v [true false]]
        (is (= {:status 200
                :body v}
               (let [app (ring/ring-handler
                           (ring/router
                             (sut/GET
                               "/my-route" []
                               :query-params [{wait_for :- (describe s/Bool "wait for patched entity to be available for search") :default}]
                               {:status 200
                                :body wait_for})
                             {:data {:middleware [parameters/parameters-middleware
                                                  rrc/coerce-request-middleware]
                                     :coercion reitit.coercion.schema/coercion}}))]
                 (app {:request-method :get
                       :uri "/my-route"
                       :query-string (str "wait_for=" v)})))))
      (testing "default"
        (let [default (gensym)]
          (is (= {:status 200
                  :body default}
                 (let [app (ring/ring-handler
                             (ring/router
                               (sut/GET
                                 "/my-route" []
                                 :query-params [{wait_for :- (describe s/Bool "wait for patched entity to be available for search") default}]
                                 {:status 200
                                  :body wait_for})
                               {:data {:middleware [parameters/parameters-middleware
                                                    rrc/coerce-request-middleware]
                                     :coercion reitit.coercion.schema/coercion}}))]
                   (app {:request-method :get
                         :uri "/my-route"}))))))
      (testing "schema failure"
        (try (let [app (ring/ring-handler
                         (ring/router
                           (sut/GET
                             "/my-route" []
                             :query-params [{wait_for :- (describe s/Bool "wait for patched entity to be available for search") :default}]
                             {:status 200
                              :body wait_for})
                           {:data {:middleware [parameters/parameters-middleware
                                                rrc/coerce-request-middleware]
                                   :coercion reitit.coercion.schema/coercion}}))]
               (app {:request-method :get
                     :uri "/my-route"
                     :query-string "wait_for=1"}))
             (is false)
             (catch Exception e
               (let [actual (-> (ex-data e)
                                (select-keys [:errors :type])
                                (update :errors update-vals su/validation-error-explain))]
                 (is (= {:errors {:wait_for (list 'not (list 'instance? java.lang.Boolean "1"))}
                         :type :reitit.coercion/request-coercion}
                        actual)))))))))

(deftest return-test
  (testing "context"
    (is-banned-macro
      `(sut/context
         "/my-route" []
         :return s/Str
         ~'routes)
      (str ":return is banned, please use :responses instead.\n"
           "In this case, :return schema.core/Str is equivalent to :responses {200 {:schema schema.core/Str}}.\n"
           "For 204, you can use :responses {204 nil}.\nFor catch-all, use :responses {:default {:schema SCHEMA}}")))
  (testing "endpoints"
    (is-banned-macro
      `(sut/GET
         "/my-route" []
         :return s/Str
         ~'routes)
      (str ":return is banned, please use :responses instead.\n"
           "In this case, :return schema.core/Str is equivalent to :responses {200 {:schema schema.core/Str}}.\n"
           "For 204, you can use :responses {204 nil}.\nFor catch-all, use :responses {:default {:schema SCHEMA}}"))))

(deftest description-test
  (testing "context"
    (is (= ["/my-route"
            {:swagger {:description "a description"}}
            [identity]]
           (sut/context
             "/my-route" []
             :description "a description"
             identity)))
    (is (= ["/my-route"
            {:swagger {:description "a description"}}
            [identity]]
           (let [descriptions-are-expressions "a description"]
             (sut/context
               "/my-route" []
               :description descriptions-are-expressions
               identity)))))
  (testing "GET"
    (is (= '["/my-route" {:get {:handler (clojure.core/fn [req__#] (clojure.core/let [] (do identity)))
                                :swagger {:description "a description"}}}]
           (dexpand-1
             `(sut/GET
                "/my-route" []
                :description "a description"
                ~'identity))))
    (is (= {:description "a description"}
           (get-in (sut/GET
                     "/my-route" []
                     :description "a description"
                     ~'identity)
                   [1 :get :swagger])))))

(deftest tags-test
  (testing "context"
    (is (= ["/my-route"
            {:swagger {:tags #{:foo :bar}}}
            [identity]]
           (sut/context
             "/my-route" []
             :tags #{:foo :bar}
             identity)))
    ;; literals only to match compojure-api's semantics
    (is (= ["/my-route"
            {:swagger {:tags 'tags-are-compile-time-literals}}
            [identity]]
           (let [tags-are-compile-time-literals #{:foo :bar}]
             (sut/context
               "/my-route" []
               :tags tags-are-compile-time-literals
               identity)))))
  (testing "GET"
    (is (= '["/my-route" {:get {:handler (clojure.core/fn [req__#] (clojure.core/let [] (do identity)))
                                :swagger {:tags (quote tags-are-compile-time-literals)}}}]
           (dexpand-1
             `(sut/GET
                "/my-route" []
                :tags ~'tags-are-compile-time-literals
                ~'identity))))
    (is (= {:tags 'tags-are-compile-time-literals}
           (get-in (sut/GET
                     "/my-route" []
                     :tags tags-are-compile-time-literals
                     identity)
                   [1 :get :swagger])))))

(deftest no-doc-test
  (testing "context"
    ;; could easily be supported if needed
    (is-banned-macro
      `(sut/context
         "/my-route" []
         :no-doc ~'an-expression
         ~'routes)
      "Not allowed these options in `context`, push into HTTP verbs instead: (:no-doc)"))
  (testing "GET"
    (is (= '["/my-route" {:get {:handler (clojure.core/fn [req__#] (clojure.core/let [] (do identity)))
                                :swagger {:no-doc an-expression}}}]
           (dexpand-1
             `(sut/GET
                "/my-route" []
                :no-doc ~'an-expression
                ~'identity))))
    (testing "literals"
      (doseq [v [true false nil]]
        (is (= `["/my-route" {:get {:handler (clojure.core/fn [~'req__#] (clojure.core/let [] (do ~'identity)))
                                    :swagger {:no-doc ~v}}}]
               (dexpand-1
                 `(sut/GET
                    "/my-route" []
                    :no-doc ~v
                    ~'identity))))))
    (let [g (gensym)]
      (is (= {:no-doc g}
             (get-in (sut/GET
                       "/my-route" []
                       :no-doc g
                       ~'identity)
                     [1 :get :swagger]))))))

(deftest summary-test
  (testing "context"
    (is (= ["/my-route"
            {:swagger {:summary "a summary"}}
            [identity]]
           (sut/context
             "/my-route" []
             :summary "a summary"
             identity)))
  (is (= ["/my-route"
          {:swagger {:summary "a summary"}}
          [identity]]
         (let [summarys-are-expressions "a summary"]
           (sut/context
             "/my-route" []
             :summary summarys-are-expressions
             identity)))))
  (testing "GET"
    (is (= '["/my-route" {:get {:handler (clojure.core/fn [req__#] (clojure.core/let [] (do identity)))
                                :swagger {:summary an-expression}}}]
           (dexpand-1
             `(sut/GET
                "/my-route" []
                :summary ~'an-expression
                ~'identity))))
    (testing "literals"
      (doseq [v ["summary" true false nil]]
        (is (= `["/my-route" {:get {:handler (clojure.core/fn [~'req__#] (clojure.core/let [] (do ~'identity)))
                                    :swagger {:summary ~v}}}]
               (dexpand-1
                 `(sut/GET
                    "/my-route" []
                    :summary ~v
                    ~'identity))))))
    (let [g (gensym)]
      (is (= {:summary g}
             (get-in (sut/GET
                       "/my-route" []
                       :summary g
                       ~'identity)
                     [1 :get :swagger]))))))

(deftest produces-test
  (testing "context"
    ;; could easily be supported if needed
    (is-banned-macro
      `(sut/context
         "/my-route" []
         :produces ~'an-expression
         ~'routes)
      "Not allowed these options in `context`, push into HTTP verbs instead: (:produces)"))
  (testing "GET"
    (is (= '["/my-route" {:get {:handler (clojure.core/fn [req__#] (clojure.core/let [] (do identity)))
                                :swagger {:produces an-expression}}}]
           (dexpand-1
             `(sut/GET
                "/my-route" []
                :produces ~'an-expression
                ~'identity))))
    (testing "literals"
      (doseq [v ["produces" true false nil]]
        (is (= `["/my-route" {:get {:handler (clojure.core/fn [~'req__#] (clojure.core/let [] (do ~'identity)))
                                    :swagger {:produces ~v}}}]
               (dexpand-1
                 `(sut/GET
                    "/my-route" []
                    :produces ~v
                    ~'identity))))))
    (let [g (gensym)]
      (is (= {:produces g}
             (get-in (sut/GET
                       "/my-route" []
                       :produces g
                       ~'identity)
                     [1 :get :swagger]))))))

(deftest scoping-difference-banned-test
  (testing "context"
    (is-banned-macro
      `(sut/context
         "/my-route" ~'req
         ~'routes)
      "Not allowed to bind anything in context, push into HTTP verbs instead: req")
    (is-banned-macro
      `(sut/context
         "/my-route" ~'[req]
         ~'routes)
      "Not allowed to bind anything in context, push into HTTP verbs instead: [req]")
    (is-banned-macro
      `(sut/context
         "/my-route" ~'{:keys [req]}
         ~'routes)
      "Not allowed to bind anything in context, push into HTTP verbs instead: {:keys [req]}"))
  (testing "GET"
    (is-banned-macro
      `(sut/GET
         "/my-route" ~'req
         :middleware ~'req
         ~'routes)
      "There is a key difference in scoping between compojure-api and our compilation to reitit. The request has been bound to req but this symbol occurs in the restructuring options. The request is not in scope here in reitit, so please rename req so this incomplete analysis can rule out this mistake.")))

(deftest query-test
  (testing "context"
    (is-banned-macro
      `(sut/context
         "/my-route" []
         :query ~'[{foo :bar :as params}
                   Schema]
         ~'routes)
      "Not allowed these options in `context`, push into HTTP verbs instead: (:query)"))
  (testing "GET"
    (testing "expansion"
      (testing "missing schema"
        (is-banned-macro
          `(sut/GET
             "/my-route" []
             :query ~'[{foo :bar :as params}]
             ~'routes)
          ":query must be a vector of length 2"))
      (testing "cannot combine with :query-params"
        (is-banned-macro
          `(sut/GET
             "/my-route" []
             :query-params [{~'wait_for :- (describe s/Bool "wait for patched entity to be available for search") nil}]
             :query ~'[{foo :bar :as params}
                       Schema]
             ~'routes)
          "Cannot use both :query-params and :query, please combine them."))
      (is (= '["/my-route" {:get {:handler (clojure.core/fn [req__#]
                                             (clojure.core/let [parameters__# (:parameters req__#)
                                                                query__# (:query parameters__#)
                                                                {foo :bar :as params} query__#]
                                               (do routes)))
                                  :parameters {:query Schema}}}]
             (dexpand-1
               `(sut/GET
                  "/my-route" []
                  :query ~'[{foo :bar :as params}
                            Schema]
                  ~'routes))))
      (testing "200 response"
        (doseq [v [true false]]
          (is (= {:status 200
                  :body v}
                 (let [app (ring/ring-handler
                             (ring/router
                               (sut/GET
                                 "/my-route" []
                                 :query [{:keys [wait_for]}
                                         (st/optional-keys
                                           {:wait_for (describe s/Bool "wait for patched entity to be available for search")})]
                                 {:status 200
                                  :body wait_for})
                               {:data {:middleware [parameters/parameters-middleware
                                                    rrc/coerce-request-middleware]
                                       :coercion reitit.coercion.schema/coercion}}))]
                   (app {:request-method :get
                         :uri "/my-route"
                         :query-string (str "wait_for=" v)}))))))
      (testing "schema failure"
        (try (let [app (ring/ring-handler
                         (ring/router
                           (sut/GET
                             "/my-route" []
                             :query [{:keys [wait_for]}
                                     (st/optional-keys
                                       {:wait_for (describe s/Bool "wait for patched entity to be available for search")})]
                             {:status 200
                              :body wait_for})
                           {:data {:middleware [parameters/parameters-middleware
                                                rrc/coerce-request-middleware]
                                   :coercion reitit.coercion.schema/coercion}}))]
               (app {:request-method :get
                     :uri "/my-route"
                     :query-string "wait_for=1"}))
             (is false)
             (catch Exception e
               (let [actual (-> (ex-data e)
                                (select-keys [:errors :type])
                                (update :errors update-vals su/validation-error-explain))]
                 (is (= {:errors {:wait_for (list 'not (list 'instance? java.lang.Boolean "1"))}
                         :type :reitit.coercion/request-coercion}
                        actual)))))))))

(deftest body-test
  (testing "context"
    (is-banned-macro
      `(sut/context
         "/my-route" []
         :body ~'[l r]
         ~'routes)
       "Not allowed these options in `context`, push into HTTP verbs instead: (:body)"))
  (testing "endpoint"
    (is-banned-macro
      `(sut/GET
         "/my-route" []
         :body ~'[{foo :bar :as params}]
         ~'routes)
      ":body must be a vector of length 2: [{foo :bar, :as params}]")
    (is (= '["/my-route" {:get {:handler (clojure.core/fn [req__#]
                                           (clojure.core/let [parameters__# (:parameters req__#)
                                                              body__# (:body parameters__#)
                                                              {foo :bar :as body} body__#]
                                             (do routes)))
                                :parameters {:body Schema}}}]
           (dexpand-1
             `(sut/GET
                "/my-route" []
                :body ~'[{foo :bar :as body}
                         Schema]
                ~'routes))))
    (testing "200 response"
      (doseq [v [true false]]
        (is (= {:status 200
                :body v}
               (let [app (ring/ring-handler
                           (ring/router
                             (sut/GET
                               "/my-route" []
                               :body [{:keys [wait_for]}
                                       (st/optional-keys
                                         {:wait_for (describe s/Bool "wait for patched entity to be available for search")})]
                               {:status 200
                                :body wait_for})
                             {:data {:middleware [parameters/parameters-middleware
                                                  rrc/coerce-request-middleware]
                                     :coercion reitit.coercion.schema/coercion}}))]
                 (app {:request-method :get
                       :uri "/my-route"
                       :body-params {:wait_for v}}))))))
    (testing "schema failure"
      (try (let [app (ring/ring-handler
                       (ring/router
                         (sut/GET
                           "/my-route" []
                           :body [{:keys [wait_for]}
                                   (st/optional-keys
                                     {:wait_for (describe s/Bool "wait for patched entity to be available for search")})]
                           {:status 200
                            :body wait_for})
                         {:data {:middleware [parameters/parameters-middleware
                                              rrc/coerce-request-middleware]
                                 :coercion reitit.coercion.schema/coercion}}))]
             (app {:request-method :get
                   :uri "/my-route"
                   :body-params {:wait_for 1}}))
           (is false)
           (catch Exception e
             (let [actual (-> (ex-data e)
                              (select-keys [:errors :type])
                              (update :errors update-vals su/validation-error-explain))]
               (is (= {:errors {:wait_for (list 'not (list 'instance? java.lang.Boolean 1))}
                       :type :reitit.coercion/request-coercion}
                      actual))))))))
