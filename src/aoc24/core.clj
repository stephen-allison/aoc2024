(ns aoc24.core)
(require '[aoc24.day1 :as day1])
(require '[aoc24.day2 :as day2])
(require '[aoc24.day3 :as day3])
(require '[aoc24.day4 :as day4])
(require '[aoc24.day5 :as day5])
(require '[aoc24.day6 :as day6])

(defn -main [& _]
  "Advent of Code 2024"
  (println "Advent of Code 2024!")
  (day1/solve)
  (day2/solve)
  (day3/solve)
  (day4/solve)
  (day5/solve)
  (day6/solve))
