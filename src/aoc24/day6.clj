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

(defn add-obstacle [obstacle-pos]
  (fn [grid pos]
    (if (= pos obstacle-pos)
      OBSTACLE
      (char-at grid pos))))

(defn find-start [grid]
  (let [positions (grid-positions grid)]
    (some #(when (= START (char-at grid %)) %) positions)))

(defn turn [direction]
  (cond (= direction UP) RIGHT
        (= direction RIGHT) DOWN
        (= direction DOWN) LEFT
        (= direction LEFT) UP))

(defn stepper [grid char-getter]
  (fn step [[[x y] [dx dy]]]
    (let [next-pos [(+ x dx) (+ y dy)]
          next-square (char-getter grid next-pos)]
      (cond (= next-square OPEN) [next-pos [dx dy]]
            (= next-square START) [next-pos [dx dy]]
            (= next-square OBSTACLE) [[x y] (turn [dx dy])]
            (= next-square EXIT) :done))))

(defn simple-path [grid char-getter]
  (let [start (find-start grid)
        stepper-fn (stepper grid char-getter)
        results (take-while #(not= :done %) (iterate stepper-fn [start UP]))]
    results))

(defn part-one [grid]
  (let [path (map first (simple-path grid char-at))]
    (count (apply hash-set path))))

(defn loop-finder [trodden next-step]
  (cond (= :done next-step) (reduced :done)
        :else (let [[pos dir] next-step]
                (if (contains? trodden [pos dir])
                  (reduced :loop)
                  (conj trodden [pos dir])))))

(defn part-two [grid]
  (let [path (map first (simple-path grid char-at))
        obstacle-fns (map add-obstacle (apply hash-set path))
        obstructed-paths (map #(simple-path grid %) obstacle-fns)]
    (count (filter #(= :loop %) (map #(reduce loop-finder #{} %) obstructed-paths)))))

(defn solve []
  (let [grid (load-map)]
    (println (part-one grid))
    (println (part-two grid))))