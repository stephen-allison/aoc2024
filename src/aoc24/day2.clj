(ns aoc24.day2
  (:require [aoc24.loader :as loader])
  (:require [aoc24.util :as util]))

(defn load-reports []
  (let [input (loader/puzzle-input-lines "day2.txt")]
    (map util/extract-numbers input)))

(defn diff [[a b]] (- b a))

(defn between [min max val] (<= min val max))

(defn all-same-sign [values]
  (or (every? pos? values)
      (every? neg? values)))

(defn safe-steps [values]
  (every? #(<= 1 (abs %) 3) values))

(defn is-safe? [report]
  (and (all-same-sign report) (safe-steps report)))

(defn diffs [values]
  (map diff (partition 2 1 values)))

(defn safe-count [judgements]
  (get (frequencies judgements) true 0))

(defn solve-part-one [reports]
  (->> reports
       (map diffs)
       (map is-safe?)
       safe-count))

(defn dampened [report]
  (for [n (range 0 (count report))]
    (concat
      (take n report)
      (drop (inc n) report))))

(defn expand [report]
  (cons report (dampened report)))

(defn safe-with-dampening? [report]
  (let [expanded (expand report)]
    (pos? (solve-part-one expanded))))

(defn solve-part-two [reports]
  (->> reports
       (map safe-with-dampening?)
       safe-count))

(defn solve []
  (println "Day Two")
  (let [reports (load-reports)]
    (println (solve-part-one reports))
    (println (solve-part-two reports))))
