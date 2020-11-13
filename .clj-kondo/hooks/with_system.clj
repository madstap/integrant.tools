(ns hooks.with-system
  (:require [clj-kondo.hooks-api :as api]))

(defn with-system [{:keys [node]}]
  (let [[binding-vec & body] (rest (:children node))
        [binding expr ks] (:children binding-vec)]
    (when-not (and binding expr)
      (throw (ex-info "No binding and/or config expr provided" {})))
    {:node (api/list-node
            (list*
             (api/token-node 'let)
             (api/vector-node [binding expr])
             ks
             body))}))
