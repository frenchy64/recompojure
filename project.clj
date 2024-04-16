(defproject com.ambrosebs/compojure-api-tools "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.2"]
                 ;; TODO remove
                 [metosin/compojure-api "1.1.13"]
                 [prismatic/schema "1.4.1"]
                 [metosin/schema-tools "0.13.1"]
                 [metosin/reitit "0.7.0-alpha7"
                  #_#_
                  :exclusions [;;FIXME compojure uses v3 and reitit uses v4.
                               metosin/ring-swagger-ui
                               metosin/reitit-malli]]]
  :repl-options {:init-ns compojure-api-tools.core-reitit})
