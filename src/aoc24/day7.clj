(ns aoc24.day7
  (:require [aoc24.loader :as loader])
  (:require [clojure.edn :as edn]))

(defn equation-data []
  (let [lines (loader/puzzle-input-lines "day7.txt")
        parsed (map #(re-seq #"\d+" %) lines)]
    (map #(map edn/read-string %) parsed)))

(defn concat-vals [a b]
  (edn/read-string (str a b)))

(defn step [acc val]
  (let [added (map #(+ val %) acc)
        multiplied (map #(* val %) acc)
        concatted (map #(concat-vals % val) acc)]
    (concat added multiplied concatted)))

(defn check-equation [eqn]
  (let [target (first eqn)
        vals (rest eqn)
        possible (reduce step [(first vals)] (rest vals))
        results (some #(when (= target %) target) possible)]
    results))