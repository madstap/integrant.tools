(ns xyz.madland.integrant.tools.exec-daemons-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.shell :as sh]
            [clj-docker-client.core :as docker]
            [uberdeps.api :as uberdeps]
            [xyz.madland.integrant.tools.exec-daemons-core]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def dconn
  {:uri "unix:///var/run/docker.sock"})

(defn clean! [id]
  (println "Cleaning")
  (time (sh/sh "rm" "-rf" "classes" "target" (str id ".txt"))))

(def core-ns
  'xyz.madland.integrant.tools.exec-daemons-core)

(defn make-jar! [id]
  (sh/sh "mkdir" "classes")
  (compile core-ns)
  (uberdeps/package (edn/read-string (slurp "deps.edn"))
                    (str "target/" id ".jar")
                    {:aliases [:test]
                     :main-class (munge core-ns)}))

(defn run-jar! [id]
  (sh/sh "bin/jar-test" id))

(deftest clean-shutdown-test
  (let [id (str (java.util.UUID/randomUUID))]
    (clean! id)
    (println "Making jar")
    (time (make-jar! id))
    (println "Running jar")
    (time (run-jar! id))
    (let [output (str/split-lines (slurp (str id ".txt")))]
      (run! println output)
      (is (= ["Start" "Hello, world!" "Stop"] (->> output (dedupe)))))
    (clean! id)))

(comment

  (require '[madstap.comfy :refer [defs]])

  (defs {{:keys [process]} :proc}
    (echo "foo" {:verbose true}))

  (.pid process)

  (docker/categories)

  (def images (docker/client {:category :images, :conn dconn}))

  (def build (docker/client {:category :build, :conn dconn}))

  (->> (docker/invoke images {:op :ImageList})
       (map :RepoTags))

  (docker/doc images :ImageCreate)

  (docker/ops images)

  (docker/ops build)

  (docker/doc build :ImageBuild)

  )
