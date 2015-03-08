(defproject gt "0.1.0-SNAPSHOT"
  :description "Graph tools. Experiments in graph theory."
  :url "http://example.com/FIXME"
  :license {:name "Unlicense (public domain)"
            :url "http://unlicense.org/"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot gt.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
