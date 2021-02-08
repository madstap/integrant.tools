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

(defmacro with-system
  {:style/indent 1}
  [[binding config ks] & body]
  (let [sys (gensym "system")]
    `(let [~sys (exec ~config ~ks)]
       (try (let ~(destructure-derived [binding sys])
              ~@body)
            (finally (ig/halt! ~sys))))))
