(defproject sprout "1.0.0-SNAPSHOT"
  :description "a test project using Leiningen"
  :dependencies [[org.clojure/clojure "1.3.0"] 
                 [clojure-twitter "1.2.5"] ; aargh, not compatible, use twitter-api
                 [org.clojure/clojure-contrib "1.2.0"]
                ]
  :dev-dependencies [
        [lein-eclipse "1.0.0"]
  ]
  :jvm-opts ["-Xmx1g" "-server"]
  :repl-init sprout.repl
  :main sprout.run
)
