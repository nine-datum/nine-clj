(defproject org.clojars.taqmuraz/nine-clj "0.1.3"
  :description "Clojure OpenGL 3D graphics library"
  :url "http://github.com/Taqmuraz/nine-clj"
  :license {:name "Creative Commons Attribution-NonCommercial-NoDerivs 4.0 International"
            :url "https://creativecommons.org/licenses/by-nc-nd/4.0/legalcode.txt"}
  :dependencies [
    [org.clojure/clojure "1.13.0-master-SNAPSHOT"]
    [net.mikera/core.matrix "0.63.0"]
    [io.github.taqmuraz/nine "1.2.40"]
    [cz.advel.jbullet/jbullet "20101010-1"]
    [zprint "1.2.9"]
  ]
  ;:jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005"]
  :main ^:skip-aot nine-clj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
)
