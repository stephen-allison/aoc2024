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

(defn part-one-file-updater [file-positions output-idx file-size file-id]
  (into file-positions
        (map vector (range output-idx (+ output-idx file-size))
                    (repeat file-size file-id))))

(defn part-two-file-updater [file-positions output-idx, file-size, file-id]
  (assoc file-positions output-idx (apply vector (repeat file-size file-id))))

(defn update-file-positions [update-fn res ch]
  (let [{file-positions :file-positions
         file-id :current-file-id
         output-idx :current-index} res
         n (edn/read-string (str ch))
        updated (update-fn file-positions output-idx n, file-id)]
    (update-scan-files res updated n)))

(defn part-one-free-updater [free-positions output-idx free-size]
  (into free-positions
        (map vector (range output-idx (+ output-idx free-size))
             (repeat free-size \.))))

(defn part-two-free-updater [free-positions output-idx free-size]
  (assoc free-positions output-idx free-size))

(defn update-free [update-fn res ch]
  (let [{free-positions :free-positions
         output-idx :current-index} res
        n (edn/read-string (str ch))
        updated (update-fn free-positions output-idx n)]
    (update-scan-free res updated n)))

(defn scan [res [index char]]
  (if (even? index)
    (update-file-positions part-two-file-updater res char)
    (update-free part-two-free-updater res char)))

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



(defn find-to-collapse [free-posns]
  (let [[_ final most] (reduce (fn [[expected rng rngs] [pos size]]
                              (cond (empty? rng) [(+ pos size) [pos] rngs]
                                    (= expected pos) [(+ pos size) (conj rng pos) rngs]
                                    :else [(+ pos size) [pos] (conj rngs rng)]))
                      [[-1 [] []]]
                    free-posns)]
    (filter #(> (count %) 1) (conj most final))))

(defn collapse-range [free-positions merging]
  (let [vals (map #(get free-positions %) merging)
        total-space (reduce + vals)
        updated (assoc free-positions (first merging) total-space)]
    (reduce (fn [mapping index] (dissoc mapping index)) updated (rest merging))))

(defn collapse [free-positions]
  (let [to-collapse (find-to-collapse free-positions)]
    (reduce collapse-range free-positions to-collapse)))

(defn move-file [[file-map free-map] [file-pos file-content]]
  (let [file-size (count file-content)
        [to-pos free-size] (first (filter (fn [[k v]] (and (>= v file-size) (< k file-pos))) free-map))]
    (if (nil? to-pos)
      [file-map free-map]
      [(move file-map file-pos to-pos)
       (collapse (part-two-free-updater
                   (move (update free-map to-pos #(- % file-size)) to-pos (+ to-pos file-size))
                   file-pos
                   file-size
                   ))])))

(defn compress-2 [res]
  (let [{file-map :file-positions
         free-pos :free-positions} res]
    (reduce move-file [file-map free-pos] (reverse file-map)))
  )

(defn checksum [compressed]
  (reduce-kv (fn [acc k v] (+ acc (* k v))) 0 compressed))

(defn checksum-part [[index vals]]
  (let [n (count vals)
        r (range index (+ index n))
        vals (map vector r vals)]
    (reduce (fn [acc [a b]] (+ acc (* a b))) 0 vals)))

(defn checksum-2 [[file-posns _]]
  (reduce + (map checksum-part file-posns)))

(defn solve [diskmap]
  (->> diskmap
       expand-diskmap
       compress
       checksum))

(defn solve-2 [diskmap]
  (->> diskmap
       expand-diskmap
       compress-2
       checksum-2))