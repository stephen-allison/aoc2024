(ns aoc24.day7
  (:require [aoc24.loader :as loader])
  (:require [clojure.edn :as edn]))

(defn equation-data
  "Load the test data, return list of lists, where
  the first value in each list is the target number and
  the rest are the inputs."
  []
  (let [lines (loader/puzzle-input-lines "day7.txt")
        parsed (map #(re-seq #"\d+" %) lines)]
    (map #(map edn/read-string %) parsed)))

(defn concat-numbers
  "Takes two numbers and returns a number that
  is the result of concatenating the two numbers."
  [a b]
  (edn/read-string (str a b)))

(defn make-stepper
  "Accumulates all the possible results of applying all the possible
  operators to each input value.  Used as a reducer."
  [fns]
  (fn [acc val]
    (let [next-vals (for [f fns] (map #(f % val) acc))]
      (apply concat next-vals))))

(defn make-checker
  "Makes a function that uses passed fns to generate all the possible
  results for a given equation input and then returns a list of those
  equations where it was possible to reach the target value."
  [fns]
  (fn [eqn]
    (let [target (first eqn)
          vals (rest eqn)
          step (make-stepper fns)
          possible (reduce step [(first vals)] (rest vals))
          results (some #(when (= target %) target) possible)]
      results)))

(defn solve-for-operators [eqns ops]
  (let [checker (make-checker ops)
        results (map checker eqns)
        ok (filter some? results)]
    (reduce + ok)))

(defn solve []
  (let [eqns (equation-data)
        part-one (solve-for-operators eqns [+ *])
        part-two (solve-for-operators eqns [+ * concat-numbers])]
    (println part-one)
    (println part-two)))
