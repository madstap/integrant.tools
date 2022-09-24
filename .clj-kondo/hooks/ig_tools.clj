(ns hooks.ig-tools
  (:require [clj-kondo.hooks-api :as hooks]))


(defn with-system [{:keys [node]}]
  (let [[binding-vec & body] (rest (:children node))
        [binding expr ks] (:children binding-vec)]
    (when-not (and binding expr)
      (throw (ex-info "No binding and/or config expr provided" {})))
    {:node (hooks/list-node
            (list*
             (hooks/token-node 'let)
             (hooks/vector-node [binding expr])
             ks
             body))}))


(defn syms-in-binding [binding]
  (let [simple-symbol (comp symbol name)]
    (->> (tree-seq coll? seq binding)
         (mapcat #(cond (and (symbol? %) (not= '& %))
                        [(simple-symbol %)]

                        (and (map-entry? %) (= :keys (key %)))
                        (->> (val %)
                             (filter keyword?)
                             (map simple-symbol)))))))


(defn defs [{:keys [node]}]
  (let [[binding expr] (rest (:children node))
        syms (syms-in-binding (hooks/sexpr binding))
        rewrite (hooks/list-node
                 (list*
                  (hooks/token-node 'let)
                  (hooks/vector-node
                   (list binding expr))
                  (map (fn [sym]
                         (hooks/list-node
                          (list
                           (hooks/token-node 'def)
                           (hooks/token-node sym)
                           (hooks/token-node sym))))
                       syms)))]
    {:node rewrite}))
