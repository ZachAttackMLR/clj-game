(defproject clj_game "0.0.1-SNAPSHOT"
  :description "Basic text based terminal game."
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]]
  :main ^:skip-aot clj_game.core
  :target-path "target/%s")
