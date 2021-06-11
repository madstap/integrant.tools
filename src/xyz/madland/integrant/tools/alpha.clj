(ns xyz.madland.integrant.tools.alpha
  (:refer-clojure :exclude [assoc, assoc-in
                            get, get-in
                            update, update-in
                            contains?
                            select-keys])
  (:require [clojure.core :as core]
            [xyz.madland.integrant.tools.state.alpha :as ig.tools.state]
            [integrant.core :as ig]))

(defn get [system k]
  (second (ig/find-derived-1 system k)))

(defn get-in [system ks]
  (-> system (get (first ks)) (core/get-in (rest ks))))

(defn find-k [system k]
  (first (ig/find-derived-1 system k)))

(defn find-ks [system k]
  (map first (ig/find-derived system k)))

(defn select-keys [system ks]
  (core/select-keys system (mapcat #(find-ks system %) ks)))

(defn get-all [system k]
  (map second (ig/find-derived system k)))

(defn assoc-all [system k v]
  (reduce #(core/assoc %1 %2 v) system (find-ks system k)))

(defn update-all [system k f & args]
  (reduce #(apply core/update %1 %2 f args) system (find-ks system k)))

(defn assoc-in-all [system ks v]
  (let [[k & more-ks] ks]
    (reduce #(core/assoc-in %1 (cons %2 more-ks) v)
            system
            (find-ks system k))))

(defn update-in-all [system ks f & args]
  (let [[k & more-ks] ks]
    (reduce #(apply core/update-in %1 (cons %2 more-ks) f args)
            system
            (find-ks system k))))

(defn assoc [system k v]
  (core/assoc system (or (find-k system k) k) v))

(defn update [system k f & args]
  (assoc system k (apply f (get system k) args)))

(defn update-in [system ks f & args]
  (let [[k & more-ks] ks]
    (if (empty? more-ks)
      (apply update system k f args)
      (apply update system k core/update-in more-ks f args))))

(defn contains? [system k]
  (not (empty? (ig/find-derived system k))))

(defn update-existing [system k f & args]
  (if (contains? system k)
    (apply update system k f args)
    system))

(defn assoc-in [system ks v]
  (let [[k & more-ks] ks]
    (if (empty? more-ks)
      (assoc system k v)
      (update system k core/assoc-in more-ks v))))

(defn init* [system ks]
  (if (some? ks) (ig/init system ks) (ig/init system)))

(defn exec [config ks]
  (-> config ig/prep (init* ks)))

(defn exec!
  ([config]
   (exec! config nil))
  ([config ks]
   (let [prepped (ig/prep config)]
     (alter-var-root #'ig.tools.state/config (constantly prepped))
     (let [system (init* prepped ks)]
       (alter-var-root #'ig.tools.state/system (constantly system))
       system))))

(defn await-shutdown! [system]
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. #(ig/halt! (cond-> system (var? system) deref))))
  (.. Thread currentThread join))

(defn exec-daemon!
  ([config]
   (exec-daemon! config nil))
  ([config ks]
   (exec! config ks)
   (await-shutdown! #'ig.tools.state/system)))

(defn destructure-derived [bindings]
  (let [[map-sym :as destructured] (destructure bindings)]
    (mapv #(cond-> %
             (and (seq? %) (= `core/get (first %)) (= map-sym (second %)))
             (-> next (conj `get)))
          destructured)))

(defmacro let-derived
  {:style/indent 1}
  [[binding system & bindings] & body]
  `(let [~@(destructure-derived [binding system])
         ~@bindings]
     ~@body))

(defmacro with-system
  {:style/indent 1}
  [[binding config ks] & body]
  (let [sys (gensym "system")]
    `(let [~sys (exec ~config ~ks)]
       (try (let-derived ~[binding sys]
              ~@body)
            (finally (ig/halt! ~sys))))))

(defn now-ms []
  (System/currentTimeMillis))

(defn wrap-init-times [init-key log]
  (fn [k config]
    (let [start (now-ms)
          ret (init-key k config)
          end (now-ms)]
      (log k (- end start))
      ret)))

(defmacro with-init-times
  "Takes a function log of the arity [k init-ms] and times invocations of
  integrant/init-key inside body. Useful for logging the startup time
  of components in a system."
  {:style/indent 1}
  [log & body]
  `(let [init-key# ig/init-key]
     (with-redefs [ig/init-key (wrap-init-times init-key# ~log)]
       ~@body)))

(defn syms-in-binding
  "Returns the symbols (and keywords that act as symbols) in a binding form."
  [binding]
  (letfn [(simple-symbol [sym]
            (with-meta (symbol (name sym)) (merge (meta binding) (meta sym))))]
    (reduce
     (fn [syms x]
       (cond-> syms
         (and (symbol? x) (not= '& x))
         (conj (simple-symbol x))

         ;; Special-case: Keywords act like symbols in {:keys [:foo :bar/baz]}
         (and (map-entry? x) (= :keys (first x)))
         (into (comp (filter keyword?)
                     (map simple-symbol)) (second x))))
     []
     (tree-seq coll? seq binding))))

(defmacro defs
  {:style/indent 1}
  [binding system]
  `(let-derived [~binding ~system]
     ~(vec (for [sym (syms-in-binding binding)]
             `(def ~sym ~sym)))))

(comment


  (defs {::keys [bar]} {[::foo ::bar] 123})

  (defs {::keys [foo]} {[::foo ::bar] 123
                        [::foo ::baz] 321})

  (def foos (get-all {[::foo ::bar] 123
                      [::foo ::baz] 321} ::foo))

  (let-derived [{::keys [foo bar]} {[::foo ::bar] 123}]
    foo)

  )
