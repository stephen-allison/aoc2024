
(ns aoc24.day3
  (:require [aoc24.loader :as loader])
  (:require [clojure.edn :as edn]))

(def ACTIVATE "do()")
(def DEACTIVATE "don't()")
(defn read-memory []
  (loader/puzzle-input "day3.txt"))

(defn line-with-numbers [[op arg1 arg2]]
  [op (edn/read-string arg1) (edn/read-string arg2)])

(defn line-op [[op _ _]] op)

(defn line-multiply [[_ arg1 arg2]] (* arg1 arg2))

(defn basic-multiplications [memory]
  (map line-with-numbers (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" memory)))

(defn multiplications-with-controls [memory]
  (map line-with-numbers (re-seq #"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)" memory)))

(defn active-lines [[lines active] line]
  (cond
    (= (line-op line) ACTIVATE) [lines true]
    (= (line-op line) DEACTIVATE) [lines false]
    :else (if (true? active)
            [(cons line lines) active]
            [lines active])
    ))

(defn activated-multiplications [operations]
  (first (reduce active-lines [[] true] operations)))

(defn part-two [memory]
  (let [ops (multiplications-with-controls memory)
        active (activated-multiplications ops)]
    (reduce + (map line-multiply active))))

(defn part-one [memory]
  (let [ops (basic-multiplications memory)]
    (reduce + (map line-multiply ops))))

(defn solve []
  (let [memory (read-memory)]
    (println "Day Three")
    (println (part-one memory))
    (println (part-two memory))))