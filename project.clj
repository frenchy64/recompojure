(defproject com.recompojure/recompojure "0.1.0-SNAPSHOT"
  :description "Compojure compiling to reitit"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :plugins [[lein-codox "0.10.7"]
            [lein-shell "0.5.0"]
            [lein-pprint "1.3.2"]]
  :profiles {:gen-doc {:jvm-opts ["--add-opens" "java.base/java.lang=ALL-UNNAMED"]}
             :dev {:dependencies [[io.github.frenchy64/fully-satisfies "1.10.4"]]}}
  :release-tasks [["clean"]
                  ["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "--no-sign"]
                  ;["shell" "./scripts/regen-latest-version-info.sh"]
                  ;["shell" "./scripts/regen-selmer.sh"]
                  ["shell" "./scripts/deploy-doc.sh"]
                  ["deploy" "release"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]]
  :deploy-repositories [["snapshot" {:url "https://clojars.org/repo"
                                     :username :env/clojars_user
                                     :password  :env/clojars_token
                                     :sign-releases false}]
                        ["release" {:url "https://clojars.org/repo"
                                    :username :env/clojars_user
                                    :password  :env/clojars_token
                                    :sign-releases false}]]

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
  :codox {:source-uri "https://github.com/frenchy64/recompojure/blob/{git-commit}/{filepath}#L{line}"}
  :repl-options {:init-ns com.recompojure.compojure-api1})
