(ns aoc24.day4
  (:require [aoc24.loader :as loader]))
(def XMAS "XMAS")
(def WORD-LEN (count XMAS))
(def UP [0 1])
(def DOWN [0 -1])
(def RIGHT [1 0])
(def LEFT [-1 0])
(def UP-RIGHT [1 -1])
(def UP-LEFT [-1 -1])
(def DOWN-LEFT [-1 1])
(def DOWN-RIGHT [1 1])
(def CENTRE [0 0])

(def DIRECTIONS [UP DOWN
                 LEFT RIGHT
                 UP-LEFT UP-RIGHT
                 DOWN-LEFT DOWN-RIGHT])

(defn grid-bounds [grid]
  [(count (first grid)) (count grid)])

(defn grid-positions [grid]
  (let [[x-max y-max] (grid-bounds grid)]
    (for [y (range 0 y-max) x (range 0 x-max)] [x y])))

(defn char-at [grid [x y]]
  (try
    (nth (nth grid y) x)
    (catch Exception _ ".")))

(defn word-on-line
  "Given a line of [x y] positions work out the corresponding word on the grid."
  [grid line]
  (apply str (map #(char-at grid %) line)))

(defn line-from
  "Given a starting [x y] position, work out all the other positions
  that form a valid line of WORD-LEN (i.e. 4) spaces in the given [dx dy] direction."
  [[x y] [dx dy]]
  (for [n (range 0 WORD-LEN)] [(+ x (* dx n)) (+ y (* dy n))]))

(defn lines-from
  "Work out all the possible lines from the given position pos."
  [pos]
  (map #(line-from pos %) DIRECTIONS))

(defn all-lines
  "Work out all the possible lines to search on the grid."
  [grid]
  (mapcat lines-from (grid-positions grid)))

(defn all-words
  "Find all the words under all the possible lines on the grid."
  [grid]
  (map #(word-on-line grid %) (all-lines grid)))

(defn cross-at [[x y]]
  (let [f (fn [[dx dy]] [(+ x dx) (+ y dy)])]
    [(map f [UP-LEFT CENTRE DOWN-RIGHT])
     (map f [UP-RIGHT CENTRE DOWN-LEFT])]))

(defn all-crosses [grid]
  (map cross-at (grid-positions grid)))

(defn is-mas? [word]
  (or (= "MAS" word) (= "SAM" word)))

(defn is-x-mas? [[word-1 word-2]]
  (and (is-mas? word-1) (is-mas? word-2)))

(defn x-words [grid cross]
  (let [[a b] cross]
    [(word-on-line grid a)
     (word-on-line grid b)]))

(defn all-x-words [grid]
  (let [crosses (all-crosses grid)]
    (map (partial x-words grid) crosses)))

(defn part-one [grid]
  (count (filter #(= XMAS %) (all-words grid))))

(defn part-two [grid]
  (count (filter is-x-mas? (all-x-words grid))))

(defn solve []
  (let [grid (loader/puzzle-input-lines "day4.txt")]
    (println "Day Four")
    (println (part-one grid))
    (println (part-two grid))))
