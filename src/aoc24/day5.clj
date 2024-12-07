(ns aoc24.day5
  (:require [aoc24.loader :as loader])
  (:require [clojure.string :as string])
  (:require [clojure.set :as set])
  (:require [clojure.edn :as edn]))

(defn puzzle-input []
  (let [raw (loader/puzzle-input-lines "day5.txt")
        raw-rules (take-while (complement empty?) raw)
        raw-seqs (rest (drop-while (complement empty?) raw))]
    [(map #(string/split % #"\|") raw-rules)
     (map #(string/split % #",") raw-seqs)]))

(defn followers [rules leader]
  (apply hash-set (for [[l f] rules :when (= l leader)] f)))

(defn leaders [rules follower]
  (for [[l f] rules :when (= f follower)] l))

(defn split-seq [s n]
  [(nth s (dec n)) (apply hash-set (drop n s))])

(defn all-splits [s]
  (for [n (range 1 (count s))] (split-seq s n)))

(defn check-split [rules [lead following]]
  (let [allowed-followers (followers rules lead)]
    (set/subset? following allowed-followers)))

(defn seq-valid [rules s]
  (let [splits (all-splits s)]
    (every? true? (map (partial check-split rules) splits))))

(defn middle-number [s]
  (edn/read-string (first (drop (int (/ (count s) 2)) s))))

(defn part-one [rules seqs]
  (reduce + (map middle-number (filter (partial seq-valid rules) seqs))))

(defn fix-ordering [rules s]
  (let [seq-set (apply hash-set s)
        counts (for [n s] [n (set/intersection seq-set (followers rules n))])
        ordered (reverse (sort-by (comp count second) counts))]
    (map first ordered)))

(defn part-two [rules seqs]
  (let [invalid (filter #(not (seq-valid rules %)) seqs)
        fixed (map (partial fix-ordering rules) invalid)]
    (reduce + (map middle-number fixed))))

(defn solve []
  (let [[rules seqs] (puzzle-input)]
    (println "Day Five")
    (println (part-one rules seqs))
    (println (part-two rules seqs))))
