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

(defn followers
  "Consult the rules to disciver which page numbers may come after
  the 'leader' page.  nb. not all of these pages may be present,
  but if page N comes after page M then it must be in the  'followers'
  of N."
  [rules leader]
  (apply hash-set (for [[l f] rules :when (= l leader)] f)))


(defn split-seq
  "Split a page sequence into [nth page {set of subsequent pages}]"
  [s n]
  [(nth s (dec n)) (apply hash-set (drop n s))])

(defn all-splits
  "Get all the splits of a page sequence.
  e.g.
  [1 2 3 4] -> [[1 {2 3 4}] [2 {3 4}] [3 {4}]]"
  [s]
  (for [n (range 1 (count s))] (split-seq s n)))

(defn check-split
  "Look up the allowed 'following' pages for a 'lead' page
  and check that all the actual following pages are a subset
  of them.  If so then that split is legal."
  [rules [lead following]]
  (let [allowed-followers (followers rules lead)]
    (set/subset? following allowed-followers)))

(defn seq-valid
  "A page sequence is valid if all its 'splits' are valid for the
  given set of rules."
  [rules s]
  (let [splits (all-splits s)]
    (every? true? (map (partial check-split rules) splits))))

(defn middle-number [s]
  (edn/read-string (first (drop (int (/ (count s) 2)) s))))

(defn part-one [rules seqs]
  (reduce + (map middle-number (filter (partial seq-valid rules) seqs))))

(defn fix-ordering
  "The rules allow each page to have a certain set of following pages. Given the
  set of pages in a candidate sequence the page that must come first is the page
  which has the highest number of other pages in the sequence in it's set
  of 'allowed' following pages. The second page has the second page the second-most
  number etc.  The last page will have no allowed followers in the sequence."
  [rules s]
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
