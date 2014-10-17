(defproject rabbiting "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.novemberain/langohr "2.11.0"]
                 [compojure "1.1.6"]
                 [ring/ring-core "1.3.1"]
                 [ring/ring-json "0.2.0"]
                 [org.clojure/clojurescript "0.0-2311"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [reagi "0.10.1"]
                 [reagent "0.4.2"]
                 [http-kit "2.1.18"]
                 ]

  :main rabbiting.client
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring-mock "0.1.5"]]}}
  :plugins [
            ;;;[lein-ring "0.8.11"]
            [lein-cljsbuild "1.0.4-SNAPSHOT"]
            [lein-pdo "0.1.1"]]
  :ring {:handler rabbiting.handler/app}
  :source-paths ["src/clj"]

  :aliases {"up" ["pdo" "cljsbuild" "auto," "run" ]}

  :cljsbuild {:builds [{:id "dev"
                      :source-paths ["src/cljs"]
                      :compiler {:preamble ["reagent/react.js"]
                                 :output-to "resources/public/js/app.js"
                                 :output-dir "resources/public/js/out"
                                 :optimizations :none
                                 :source-map true}}]}
  )
