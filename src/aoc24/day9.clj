(ns aoc24.day9
  (:require [aoc24.loader :as loader])
  (:require [clojure.edn :as edn]))

;
; input: string 2333133121414131402
;
; output: map of files {positon: file-id}
;         list of free spaces [a b c]
;
(defn scan-data
  ([]
   {:file-positions  (sorted-map)
    :free-positions  []
    :current-index   0
    :current-file-id 0})
  )

(defn update-scan-files [res updated n]
  {:file-positions  updated
   :free-positions  (:free-positions res)
   :current-index   (+ (:current-index res) n)
   :current-file-id (inc (:current-file-id res))})

(defn update-scan-free [res updated n]
  {:file-positions  (:file-positions res)
   :free-positions  updated
   :current-index   (+ (:current-index res) n)
   :current-file-id (:current-file-id res)})

(defn update-file-positions [res input-idx ch]
  (let [file-positions (:file-positions res)
        file-id (:current-file-id res)
        n (edn/read-string (str ch))
        output-idx (:current-index res)
        updated (into file-positions (map vector (range output-idx (+ output-idx n)) (repeat n file-id)))]
    (update-scan-files res updated n)))

(defn update-free [res idx ch]
  (let [free-positions (:free-positions res)
        n (edn/read-string (str ch))
        output-idx (:current-index res)
        updated (into free-positions (range output-idx (+ output-idx n)))]
    (update-scan-free res updated n)))

(defn scan [res [index char]]
  (if (even? index)
    (update-file-positions res index char)
    (update-free res index char)))

(defn expand-diskmap [diskmap]
  (reduce scan (scan-data) (map-indexed vector diskmap)))

(defn move
  ([mapping from-index to-index]
    (let [value (get mapping from-index)]
      (dissoc (assoc mapping to-index value) from-index)))
  ([mapping [from-index to-index]]
    (move mapping from-index to-index)))

(defn compress [res]
  (let [file-map (:file-positions res)
        free-pos (:free-positions res)
        moves (take-while (fn [[a b]] (> a b))
                         (map vector (reverse (keys file-map)) free-pos))]
      (reduce move file-map moves)))

(defn checksum [compressed]
  (reduce-kv (fn [acc k v] (+ acc (* k v))) 0 compressed))

(defn solve [diskmap]
  (->> diskmap
       expand-diskmap
       compress
       checksum
       ))