(ns hooks.madland.integrant.tools.alpha
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

(defn syms-in-binding
  "Returns the symbols (and keywords that act as symbols) in a binding form."
  [binding]
  (letfn [(simple-symbol [sym]
            (with-meta (symbol (name sym)) (merge (meta binding) (meta sym))))]
    (reduce
     (fn [acc x]
       (cond (and (symbol? x) (not= '& x))
             (conj acc (simple-symbol x))

             ;; Special-case: Keywords act like symbols in {:keys [:foo :bar/baz]}
             (and (map-entry? x) (= :keys (first x)))
             (into acc (comp (filter keyword?)
                             (map simple-symbol)) (second x))

             :else acc))
     []
     (tree-seq coll? seq binding))))

(defn defs [{:keys [node]}]
  (let [[_ binding _] (api/sexpr node)]
    {:node (api/list-node
            (list* (api/token-node 'do)
                   (map #(api/list-node
                          (list
                           (api/token-node 'def)
                           (api/token-node %)
                           (api/token-node nil)))
                        (syms-in-binding binding))))}))
