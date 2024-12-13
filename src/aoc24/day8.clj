(ns aoc24.day8
  (:require [aoc24.loader :as loader]))

(def EMPTY \.)

(defn gcd
  "Calculate the greatest common divisor of a and b.
  https://en.wikipedia.org/wiki/Greatest_common_divisor#Euclidean_algorithm"
  [a b]
  (loop [a a b b]
    (if (= b 0)
      (abs a)
      (recur b (mod a b)))))

(defn load-positions []
  (let [input (loader/puzzle-input-lines "day8.txt")
        x-max (count (first input))
        y-max (count input)
        get-char (fn [[x y]] (nth (nth input y) x))
        positions (for [y (range y-max) x (range x-max)] [x y])
        build-map (fn [m coord]
                    (let [char-at-coord (get-char coord)
                          char-locs (get m char-at-coord [])]
                      (if (not= char-at-coord EMPTY)
                        (assoc m char-at-coord (conj char-locs coord))
                        m)))]
    [x-max y-max (reduce build-map (hash-map) positions)]))

(defn all-pairs [positions]
  (for [a positions b positions :when (not= a b)] [a b]))

(defn component-wise-add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn component-wise-negate [[x y]]
  [(- x) (- y)])

(defn scalar-multiply [n [x y]]
  [(* x n) (* y n)])

(defn distance [[x1 y1] [x2 y2]]
  [(- x2 x1) (- y2 y1)])

(defn make-bounds-checker [x-max y-max]
  (fn [[x y]] (and (<= 0 x (dec x-max)) (<= 0 y (dec y-max)))))

(defn external-nodes [a b]
  (let [d (distance a b)]
    [(component-wise-add b d)
     (component-wise-add a (component-wise-negate d))]))

(defn internal-nodes [a b]
  (let [[dx dy] (distance a b)
        dx-by-3 (/ dx 3)
        dy-by-3 (/ dy 3)
        has-internal (and (int? dx-by-3) (int? dy-by-3))]
    (if has-internal [(component-wise-add a [dx-by-3 dy-by-3])
                      (component-wise-add b (component-wise-negate [dx-by-3 dy-by-3]))])))

(defn nodes [a b]
  (concat (external-nodes a b) (internal-nodes a b)))

(defn node-finder [bounds-check]
  (fn [acc _ antennae]
    (let [antenna-pairs (all-pairs antennae)
          node-positions (apply hash-set (mapcat (fn [[a b]] (nodes a b)) antenna-pairs))
          in-bounds-nodes (filter bounds-check node-positions)]
      (apply conj acc in-bounds-nodes))))

(defn unit-step [a b]
  (let [[dx dy] (distance a b)
        hcf (gcd dx dy)]
    (cond (= 0 dx) [0 (/ dy (abs dy))]
          (= 0 dy) [(/ dx (abs dx)) 0]
          :else [(/ dx hcf) (/ dy hcf)])))

(defn inline-node-finder [bounds-check]
  (let [in-line-nodes (fn [a b]
                        (let [step (unit-step a b)]
                          (concat (take-while bounds-check
                                              (for [p (iterate inc 0)]
                                                (component-wise-add a (scalar-multiply p step))))
                                  (take-while bounds-check
                                              (for [p (iterate dec -1)]
                                                (component-wise-add a (scalar-multiply p step)))))))]
    (fn [acc _ antennae]
      (let [antenna-pairs (all-pairs antennae)
            node-positions (apply hash-set (mapcat (fn [[a b]] (in-line-nodes a b)) antenna-pairs ))]
        (apply conj acc node-positions)))))

(defn solve []
  (let [[x-max y-max antenna-map] (load-positions)
        bounds-checker (make-bounds-checker x-max y-max)
        finder-1 (node-finder bounds-checker)
        part-1 (count (reduce-kv finder-1 #{} antenna-map))
        finder-2 (inline-node-finder bounds-checker)
        part-2 (count (reduce-kv finder-2 #{} antenna-map))]
    (println "Day Eight")
    (println part-1)
    (println part-2)))



