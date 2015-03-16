(defproject gt "0.2.0-SNAPSHOT"
  :description "Graph tools. Experiments in graph theory."
  :license {:name "Unlicense (public domain)"
            :url "http://unlicense.org/"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [http-kit "2.1.16"]
                 [compojure "1.3.2"]
                 [hiccup "1.0.5"]
                 [ring/ring-defaults "0.1.4"]
                 [ch.qos.logback/logback-classic "1.0.9"]
                 [org.clojure/tools.logging "0.3.1"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
