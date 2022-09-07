(ns xyz.madland.integrant.tools.alpha
  (:refer-clojure :exclude [assoc, assoc-in
                            get, get-in
                            update, update-in
                            contains?
                            select-keys])
  (:require [clojure.core :as core]
            [xyz.madland.integrant.tools.state.alpha :as ig.tools.state]
            [integrant.core :as ig])
  (:import (java.util Date)))

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

(defn assoc
  ([system k v]
   (core/assoc system (or (find-k system k) k) v))
  ([system k v & kvs]
   (reduce (fn [sys [k' v']] (assoc sys k' v'))
           (assoc system k v)
           (partition 2 kvs))))

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

(defn await-shutdown! [system]
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. #(ig/halt! (cond-> system (var? system) deref))))
  (.. Thread currentThread join))

(defn exec-daemon!
  ([config]
   (exec-daemon! config nil))
  ([config ks]
   (alter-var-root #'ig.tools.state/config (constantly config))
   (let [system (exec config ks)]
     (alter-var-root #'ig.tools.state/system (constantly system))
     (await-shutdown! #'ig.tools.state/system))))

(defn destructure-derived [bindings]
  (let [[map-sym :as destructured] (destructure bindings)]
    (mapv #(cond-> %
             (and (seq? %) (= `core/get (first %)) (= map-sym (second %)))
             (-> next (conj `get)))
          destructured)))

(defmacro let-derived [bindings & body]
  `(let ~(destructure-derived bindings)
     ~@body))

(defmethod ig/init-key ::const [_ v] v)

(defn merge-overrides [sys overrides]
  (reduce-kv (fn [sys k v]
               (-> sys (dissoc (find-k sys k)) (assoc [::const k] v)))
             sys
             overrides))

(defmacro with-system
  {:style/indent 1}
  [[binding config ks & {:keys [overrides]}] & body]
  (let [sys (gensym "system")]
    `(let [~sys (exec (merge-overrides ~config ~overrides) ~ks)]
        (try (let-derived ~[binding sys]
              ~@body)
             (finally (ig/halt! ~sys))))))

(defn syms-in-binding
  "Returns the symbols (and keywords that act as symbols) in a binding form."
  {:no-doc true}
  [binding]
  (let [simple-symbol (comp symbol name)]
    (->> (tree-seq coll? seq binding)
         (mapcat #(cond (and (symbol? %) (not= '& %))
                        [(simple-symbol %)]

                        (and (map-entry? %) (= :keys (key %)))
                        (->> (val %)
                             (filter keyword?)
                             (map simple-symbol)))))))

(defmacro defs
  {:style/indent 1}
  [binding body]
  `(let-derived ~[binding body]
     ~@(for [sym (syms-in-binding binding)]
         `(def ~sym ~sym))
     nil))

(defn now-ms []
  (.getTime (Date.)))

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
