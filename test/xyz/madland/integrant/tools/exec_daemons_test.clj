(ns xyz.madland.integrant.tools.exec-daemons-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.shell :as sh]
            [clojure.java.io :as io]
            [clj-docker-client.core :as docker]
            [uberdeps.uberjar :as uberjar]
            [uberdeps.api :as uberdeps]
            [xyz.madland.integrant.tools.exec-daemons-core]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [me.raynes.conch :as conch]))

(def dconn
  {:uri "unix:///var/run/docker.sock"})

(defn clean! []
  (sh/sh "rm" "-r" "classes" "target" "foo.txt"))

(def core-ns
  'xyz.madland.integrant.tools.exec-daemons-core)

;;

(defn make-jar! []
  (clean!)
  (sh/sh "mkdir" "classes")
  (compile core-ns)
  (uberdeps/package (edn/read-string (slurp "deps.edn"))
                    "target/foo.jar"
                    {:aliases [:test]
                     :main-class (munge core-ns)}))


(comment

  (clean!)

  (make-jar!)

  ;; $ java -jar target/foo.jar >> foo.txt
  ;; $ kill $(ps -ef | grep foo.jar | grep -iv grep | awk '{print $2}')

  ;; java -jar target/foo.jar >> foo.txt & sleep 3 && kill $(ps -ef | grep foo.jar | grep -iv grep | awk '{print $2}')

  (sh/sh "bash" "java -jar target/foo.jar >> foo.txt & sleep 3 && kill $(ps -ef | grep foo.jar | grep -iv grep | awk '{print $2}')")

  (= ["Start" "Hello, world!" "Stop"]
     (->> (str/split-lines (slurp "foo.txt")) (distinct)))




  ;; repl

  (java "-jar" "target/foo.jar" {:out (java.io.File. "foo.txt")})

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
