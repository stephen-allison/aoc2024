(ns aoc24.day9
  (:require [aoc24.loader :as loader])
  (:require [clojure.edn :as edn]))

(defn process
  ([[step-no value]]
   (process step-no value))
  ([step-no value]
   (let [repeat-count (edn/read-string (str value))]
     (if (even? step-no)
       (apply str (repeat repeat-count (str (/ step-no 2))))
       (apply str (repeat repeat-count "."))))))

(defn expand-diskmap [diskmap]
  (let [zipped (map-indexed vector diskmap)]
    (apply str (map process zipped))))

(defn is-digit? [c]
  (Character/isDigit c))

(defn digit-locations [s]
  (->> (map-indexed vector s)
       (filter #(is-digit? (second %)))
       (into [])))

(defn dot-locations [s]
  (->> (map-indexed vector s)
       (filter #(= \. (second %)))
       (map first)
       reverse
       (into [])))

(defn compress [diskmap]
  (let [digit-locs (digit-locations diskmap)
        free-locs (dot-locations diskmap)
        diskmap-transient! (transient (apply vector diskmap))]
    (loop [digit-locs digit-locs
           free-locs free-locs
           diskmap! diskmap-transient!]
      (let
        [[digit-index digit] (peek digit-locs)
         free-index (peek free-locs)]
        (if (> free-index digit-index)
          (persistent! diskmap!)
          (recur (pop digit-locs)
                 (pop free-locs)
                 (assoc! (assoc! diskmap! free-index digit) digit-index \.)))))))

(defn checksum [diskmap]
  (->> (second (re-matches #"(\d+)\.*" diskmap))
       (map-indexed vector)
       (map (fn [[idx num]] (* idx (edn/read-string (str num)))))
       (reduce +)))

(defn solve [diskmap]
  (->> diskmap
       expand-diskmap
       (apply vector)
       compress
       ;checksum
       ))