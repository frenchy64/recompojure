(ns com.recompojure.compojure-api1-extension-test
  (:require [com.recompojure.compojure-api1.impl :as impl]
            [clojure.set :as set]
            [com.recompojure.compojure-api1-test :refer [is-banned-macro dexpand-1]]
            [clojure.test :refer [is]]
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

(defn ident->map-stub [& args] {:op `ident->map-stub :args args})
(defn check-return-banned! [options]
  (when-some [[_ schema] (find options :return)]
    (throw (ex-info (format (str ":return is banned, please use :responses instead.\n"
                                 "In this case, :return %s is equivalent to :responses {200 {:schema %s}}.\n"
                                 "For 204, you can use :responses {204 nil}.\n"
                                 "For catch-all, use :responses {:default {:schema SCHEMA}}")
                            schema schema)
                    {}))))

(def ^:private options {:impl :compojure-api1
                        :restructure-compojure
                        {:capabilities
                         (fn [{:keys [capabilities]} _]
                           {:retit-map [{[:middleware]
                                         [`(wrap-capabilities-stub ~capabilities)]}]})
                         :return (fn [options _] (check-return-banned! options))
                         :identity-map
                         (fn [{:keys [identity-map]} _]
                           (when identity-map
                             (assert (simple-symbol? identity-map)
                                     (str ":identity-map must be a simple symbol: "
                                          (pr-str identity-map)))
                             {:scoped [{:name identity-map
                                        :op :get-in-request
                                        :path [:identity]
                                        :wrap #(list `ident->map-stub %)}]}))}
                        :extra-allowed-endpoint-options #{:capabilities :auth-identity :identity-map}})

(impl/load-api `options)

(deftest auth-identity-test
  (testing "context"
    (is-banned-macro
      `(context
         "/my-route" []
         :auth-identity ~'identity
         ~'routes)
      "Not allowed these options in `context`, push into HTTP verbs instead: (:auth-identity)"))
  (testing "GET"
    (testing "expansion"
      (is (= '["/my-route" {:get {:handler (clojure.core/fn [{scoped-identity :identity}]
                                             clojure.core/identity)}}]
             (dexpand-1
               `(GET
                  "/my-route" []
                  :auth-identity ~'scoped-identity
                  identity)))))
    (testing "200 response"
      (let [id ::read-only-identity #_(->ReadOnlyIdentity)
            response (let [app (ring/ring-handler
                                 (ring/router
                                   (GET "/my-route" []
                                        :auth-identity scoped-identity
                                        {:status 200
                                         :body scoped-identity})))]
                       (app {:request-method :get
                             :uri "/my-route"
                             :identity id}))]
        (is (= {:status 200
                :body id}
               response))
        (is (identical? id (:body response)))))))

(deftest identity-map-test
  (testing "context"
    (is-banned-macro
      `(context
         "/my-route" []
         :identity-map ~'identity-map
         ~'routes)
      "Not allowed these options in `context`, push into HTTP verbs instead: (:identity-map)"))
  (testing "GET"
    (testing "expansion"
      (is (= '["/my-route" {:get {:handler (clojure.core/fn [{scoped-identity-map__# :identity}]
                                             (clojure.core/let
                                               [scoped-identity-map
                                                (com.recompojure.compojure-api1.impl/ident->map-stub scoped-identity-map__#)]
                                               clojure.core/identity))}}]
             (dexpand-1
               `(GET
                  "/my-route" []
                  :identity-map ~'scoped-identity-map
                  identity))))
      (testing "with auth-identity, shares :identity"
        (is (= '["/my-route" {:get {:handler (clojure.core/fn [req__#]
                                               (clojure.core/let [identity__# (:identity req__#)
                                                                  scoped-identity identity__#
                                                                  scoped-identity-map (ctia.auth/ident->map identity__#)]
                                                 (do clojure.core/identity)))}}]
               (dexpand-1
                 `(GET
                    "/my-route" []
                    :identity-map ~'scoped-identity-map
                    :auth-identity ~'scoped-identity
                    identity))))))
    (testing "200 response"
      (let [id ::read-only-identity #_(->ReadOnlyIdentity)]
        (is (= {:status 200
                :body {:op 'com.recompojure.compojure-api1.impl/ident->map-stub
                       :args [:com.recompojure.compojure-api1-extension-test/read-only-identity]}}
               (let [app (ring/ring-handler
                           (ring/router
                             (GET "/my-route" []
                                  :identity-map scoped-identity-map
                                  {:status 200
                                   :body scoped-identity-map})))]
                 (app {:request-method :get
                       :uri "/my-route"
                       :identity id}))))))))
