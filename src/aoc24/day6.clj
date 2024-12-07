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

(defn grid-positions
  "List all the valid [x y] positions on the grid"
  [grid]
  (let [[x-max y-max] (grid-bounds grid)]
    (for [y (range 0 y-max) x (range 0 x-max)] [x y])))

(defn char-at
  "Get the character at a given [x y] position on the grid.
  If teh supplied [x y] is out of bounds return EXIT to indicate
  the guard has moved out of the mapped area"
  [grid [x y]]
  (try
    (nth (nth grid y) x)
    (catch Exception _ EXIT)))

(defn add-obstacle
  "Creates a function that returns the character at a given [x y]
  position but if that position is the same as obstacle-position
  will return OBSTACLE instead of what is represented in the grid"
  [obstacle-pos]
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

(defn stepper
  "Creates a function that will move the guard to its next position
  in the given grid. char-getter is used to resolve the character at the
  position the guard will try to move into"
  [grid char-getter]
  (fn step [[[x y] [dx dy]]]
    (let [next-pos [(+ x dx) (+ y dy)]
          next-square (char-getter grid next-pos)]
      (cond (= next-square OPEN) [next-pos [dx dy]]
            (= next-square START) [next-pos [dx dy]]
            (= next-square OBSTACLE) [[x y] (turn [dx dy])]
            (= next-square EXIT) :done))))

(defn path-of-guard
  "Generates a path through the grid ending when the path leaves the bounds
  of the grid. Won't terminate if there is a loop in the path. Results are a sequence
  of [x y] [dx dy] where [x y] is a position in the grid and [dx dy] is the direction
  of travel (see UP DOWN etc above)."
  [grid char-getter]
  (let [start (find-start grid)
        stepper-fn (stepper grid char-getter)
        results (take-while #(not= :done %) (iterate stepper-fn [start UP]))]
    results))

(defn part-one [grid]
  (let [path (map first (path-of-guard grid char-at))]
    (count (apply hash-set path))))

(defn loop-finder
  "Used as a reducer. Given a lazy path this will detect if an [x y] position
  has been passed through before when heading in the same direction. If this
  happens we have a loop and the reduce will end with :loop.  If the path
  leaves the bounds of the grid the reduce will end with :done"
  [trodden next-step]
  (cond (= :done next-step) (reduced :done)
        :else (let [[pos dir] next-step]
                (if (contains? trodden [pos dir])
                  (reduced :loop)
                  (conj trodden [pos dir])))))

(defn part-two
  "First, calculate the guard's path in the unchanged grid since only these locations
  are interesting candidates for adding obstacles. For each of these locations we
  create an obstacle function that will feed OBSTACLE to the path navigation routine
  at the appropriate point.  We run this for every possible OBSTACLE location and count
  how many of the paths are loops."
  [grid]
  (let [path (map first (path-of-guard grid char-at))
        obstacle-fns (map add-obstacle (apply hash-set path))
        obstructed-paths (map #(path-of-guard grid %) obstacle-fns)]
    (count (filter #(= :loop %) (map #(reduce loop-finder #{} %) obstructed-paths)))))

(defn solve []
  (let [grid (load-map)]
    (println (part-one grid))
    (println (part-two grid))))