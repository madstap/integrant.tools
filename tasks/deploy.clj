#!/usr/bin/env bb
(ns deploy
  (:require
   [rewrite-clj.zip :as zip]
   [clojure.string :as str]
   [babashka.process :as proc]
   [babashka.tasks :as tasks]))

(defn on-main? []
  (-> (proc/sh ["git" "branch" "--show-current"]) :out str/trim (= "main")))

(defn clean-workdir? []
  (-> (proc/sh ["git" "status" "--porcelain"]) :out str/blank?))

(defn zip-assoc-in [zloc ks v]
  (-> (reduce zip/get zloc (butlast ks))
      (zip/assoc (last ks) v)))

(defn update-version [version]
  (let [new-deps-edn
        (-> (zip/of-string (slurp "deps.edn"))
            (zip-assoc-in [:aliases :jar :exec-args :version] version)
            (zip/root-string))]
    (spit "deps.edn" new-deps-edn)))

(defn commit-and-push [version]
  (tasks/shell "git add .")
  (tasks/shell (str "git commit -m  'Release version "  version "'"))
  (tasks/shell "git push"))
