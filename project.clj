(defproject my-stuff "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.11.0"]]
  :main core
  :target-path "target/%s"
  :plugins [[lein-auto "0.1.3"]]
  :profiles {:uberjar {:aot :all}})
