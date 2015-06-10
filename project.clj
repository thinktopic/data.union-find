(defproject org.jordanlewis/data.union-find "0.1.1-thinktopic"
            :description "Persistent disjoint-set forests using Tarjan's union-find algorithm."
            :url "http://github.com/jordanlewis/data.union-find"
            :license {:name "Eclipse Public License"
                      :url "http://www.eclipse.org/legal/epl-v10.html"}
            :dependencies [[org.clojure/clojure "1.6.0"]]

            :profiles
            {:dev {:source-paths      ["src" "dev"]
                   :dependencies [[criterium "0.4.3"]]}}
            :plugins [[s3-wagon-private "1.1.2"]]
            :repositories  {"snapshots"  {:url "s3p://thinktopic.jars/snapshots/"
                                :passphrase :env
                                :username :env}
                  "releases"  {:url "s3p://thinktopic.jars/releases/"
                               :passphrase :env
                               :username :env}}

            )
