(ns aoc24.day6
  (:require [aoc24.loader :as loader]))

(def OBSTACLE \#)
(def EXIT \x)
(def OPEN \.)
(def START \^)
(def UP [0 -1])
(def DOWN [0 1])
(def RIGHT [1 0])
(def LEFT [-1 0])

(defn load-map []
  (loader/puzzle-input-lines "day6.txt"))

(defn grid-bounds [grid]
  [(count (first grid)) (count grid)])

(defn grid-positions [grid]
  (let [[x-max y-max] (grid-bounds grid)]
    (for [y (range 0 y-max) x (range 0 x-max)] [x y])))

(defn char-at [grid [x y]]
  (try
    (nth (nth grid y) x)
    (catch Exception _ EXIT)))

(defn find-start [grid]
  (let [positions (grid-positions grid)]
    (some #(when (= START (char-at grid %)) %) positions)))

(defn turn [direction]
  (cond (= direction UP) RIGHT
        (= direction RIGHT) DOWN
        (= direction DOWN) LEFT
        (= direction LEFT) UP))

(defn step [[grid [x y] [dx dy]]]
  (let [next-pos [(+ x dx) (+ y dy)]
        next-square (char-at grid next-pos)]
    (cond (= next-square OPEN) [grid next-pos [dx dy]]
          (= next-square START) [grid next-pos [dx dy]]
          (= next-square OBSTACLE) [grid [x y] (turn [dx dy])]
          (= next-square EXIT) :done)))

(defn part-one [grid]
  (let [start (find-start grid)
        results (take-while #(not= :done %) (iterate step [grid start UP]))
        path (map second results)]
    (count (apply hash-set path))))

(defn loop-finder [trodden next-step]
  (cond (= :done next-step) (reduced [:done trodden])
        :else (let [[_ pos dir] next-step]
                (if (contains? trodden [pos dir])
                  (reduced [:loop trodden]))
                  (conj trodden [pos dir]))))



(defn part-two [grid]
  (let [start (find-start grid)
        results (iterate step [grid start UP])]
    results))

(defn solve []
  (let [grid (load-map)]
    (part-one grid)))