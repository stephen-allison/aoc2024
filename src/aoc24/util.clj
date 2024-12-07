(ns aoc24.util
  (:require [clojure.string :as cs])
  (:require [clojure.edn :as edn]))

(defn extract-numbers [input-line]
  (map edn/read-string (cs/split input-line #"\s+")))
