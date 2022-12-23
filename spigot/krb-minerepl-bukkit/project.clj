(defproject krb-minerepl-bukkit "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :repl-options {:init-ns krb-minerepl-bukkit.core}
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]
  :resource-paths ["src/main/resources" "lib/spigot-1.14.4.jar"]
  :manifest {
             "Class-Path" "./"
             }
  :dependencies [
                 [org.clojure/clojure              "1.10.0"]
                 [org.clojure/tools.logging        "0.4.1"]
                 [org.clojure/tools.nrepl          "0.2.13"]
                 [cider/cider-nrepl                "0.21.1"]
                 [ch.qos.logback/logback-classic   "1.2.3"]
                 ;; [org.spigotmc/spigot-api          "1.9"]
                 [org.spigotmc/spigot-api          "1.19.3-R0.1-SNAPSHOT"]
                 ])
