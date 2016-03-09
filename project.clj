(defproject filmania "0.1.0-SNAPSHOT"
  :description "Des films, du data, de la science... 
Du data-science pour les films..."
  :url "https://github.com/kuoa/clojure-movie-stats"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/data.csv "0.1.3"]]
  :main ^:skip-aot filmania.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})


