(ns aoc24.day1
  (:require [aoc24.loader :as loader])
  (:require [aoc24.util :as util]))

(def unzip (juxt (partial map first) (partial map second)))

(defn distance [[a b]] (abs (- a b)))

(defn load-lists []
  (let [input (loader/puzzle-input-lines "day1.txt")]
    (map util/extract-numbers input))
  )

(defn similarity [list-1 list-2]
  (let [counts (frequencies list-2)]
    (map #(* % (get counts % 0)) list-1))
  )

(defn solve-part-one [pairs]
  (->> pairs
       unzip
       (map sort)
       (apply map vector)
       (map distance)
       (reduce +)))

(defn solve-part-two [pairs]
  (->> pairs
       unzip
       (apply similarity)
       (reduce +)))

(defn solve []
  (println "Day One")
  (let [puzzle-data (load-lists)]
    (println (solve-part-one puzzle-data))
    (println (solve-part-two puzzle-data))))