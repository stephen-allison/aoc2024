(ns aoc24.loader)
(require 'clojure.java.io)
(require 'clojure.string)

(defn puzzle-input [filename]
  (let [path (.getPath (clojure.java.io/resource filename))]
    (do
      (slurp path))))

(defn puzzle-input-lines [filename]
  (clojure.string/split-lines (puzzle-input filename)))
