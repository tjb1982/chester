(defproject chester "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.codec "0.1.0"]
                 [org.clojure/java.jdbc "0.7.0-alpha1"]
                 [co.nclk/clj-yaml "0.1.0-SNAPSHOT"]
                 [cheshire "5.6.3"]
                 [compojure "1.5.1"]
                 [ring/ring-codec "1.0.1"]
                 [org.postgresql/postgresql "9.4.1212"]
                ]
  :profiles {:dev
             {:dependencies [[ring/ring-mock "0.3.0"]
                             [ring/ring-defaults "0.2.1"]]}}
  :main chester.core
  :aot [chester.APIException]
  )
