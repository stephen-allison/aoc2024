(ns aoc24.day9
  (:require [clojure.edn :as edn]))

;
; input: string 2333133121414131402
;
; output: map of files {positon: file-id}
;         list of free spaces [a b c]
;
(defn new-scan []
  {:file-positions  (sorted-map)
   :free-positions  (sorted-map)
   :current-index   0
   :current-file-id 0})

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

(defn update-file-positions [res ch]
  (let [{file-positions :file-positions
         file-id :current-file-id
         output-idx :current-index} res
        n (edn/read-string (str ch))
        updated (into file-positions (map vector (range output-idx (+ output-idx n)) (repeat n file-id)))]
    (update-scan-files res updated n)))

(defn update-free [res ch]
  (let [{free-positions :free-positions
         output-idx :current-index} res
        n (edn/read-string (str ch))
        updated (into free-positions (map vector (range output-idx (+ output-idx n)) (repeat n \.)))]
    (update-scan-free res updated n)))

(defn scan [res [index char]]
  (if (even? index)
    (update-file-positions res char)
    (update-free res char)))

(defn expand-diskmap [diskmap]
  (reduce scan (new-scan) (map-indexed vector diskmap)))

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
                         (map vector (reverse (keys file-map)) (keys free-pos)))]
      (reduce move file-map moves)))

(defn checksum [compressed]
  (reduce-kv (fn [acc k v] (+ acc (* k v))) 0 compressed))

(defn solve [diskmap]
  (->> diskmap
       expand-diskmap
       compress
       checksum))