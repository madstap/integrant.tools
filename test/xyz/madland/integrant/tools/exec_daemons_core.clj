(ns xyz.madland.integrant.tools.exec-daemons-core
  (:gen-class)
  (:require
   [integrant.core :as ig]
   [xyz.madland.integrant.tools.alpha :as ig.tools]))

(defmethod ig/init-key ::helloworld [_ {:keys [interval]}]
  (let [*running? (atom true)]
    (println "Start")
    (.start (Thread. (fn []
                       (Thread/sleep interval)
                       (when @*running?
                         (println "Hello, world!")
                         (recur)))))
    *running?))

(defmethod ig/halt-key! ::helloworld [_ *running?]
  (reset! *running? false)
  (Thread/sleep 2)
  (println "Stop"))

(def config
  {::helloworld {:interval 100}})

(defn -main []
  (ig.tools/exec-daemon! config))
