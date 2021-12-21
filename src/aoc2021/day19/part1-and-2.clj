(ns aoc2021.day19.part1
  (:require [aoc2021.util :as util]
            [clojure.string :as s]
            [clojure.core.matrix :as m]
            [clojure.set :as st])
  (:import [java.util UUID]))

;; ALGORITHM EXPLANATION:
;; - For each scanner, I compute the differences between its pairs of beacons,
;;   and then store these as sets of absolute values: if (beacon_a - beacon_b) = [x, y, z],
;;   I will store #{abs(x), abs(y), abs(z)}. I call these sets "diffs".
;; - For each pair of scanner, I check if they have at least 66 "diffs" in common.
;;   If yes, it means (unless a malicious input has been hand-crafted) that these two scanners
;;   have 12 beacons in common, and so I resolve which beacons of scanner A correspond
;;   to which beacons of scanner B.
;; - From the previous step, I have constructed a data structure that gives me
;;   classes of equivalence between beacons, i.e. what beacons from different scanners
;;   are actually the same beacon. I can solve part 1 of the problem directly from here
;;   (without even computing the actual positions of the scanners and beacons), since
;;   I can just count the total number of beacons in the original input and substract the
;;   number of duplicates identified with my equivalence data structure.
;; - For part 2 however, I have to find the actual positions of all the scanners. For this,
;;   I compute the relative rotation and translation between pairs of scanners with overlapping
;;   beacons (by finding the right transform that maps the coordinates of that beacon from
;;   scanner A's system to scanner B's system).
;; - Finally I can resolve the absolute coordinates of each scanner by doing a graph traversal
;;   and combining the relative transforms from one scanner to another. Obtaining the largest
;;   Manhattan distance is then easy.

(defn parse-input [input]
  (loop [[line & rest] input
         scanners []]
    (if (nil? line)
      scanners
      (if (s/starts-with? line "--- scanner")
        (recur rest (conj scanners []))
        (if (empty? line)
          (recur rest scanners)
          (let [scanner (peek scanners)
                point (mapv #(Integer/parseInt %) (s/split line #","))
                scanners (conj (pop scanners) (conj scanner point))]
            (recur rest scanners)))))))


(def R-id [[1 0 0] [0 1 0] [0 0 1]])
(def Rx-90 [[1 0 0] [0 0 -1] [0 1 0]])
(def Ry-90 [[0 0 1] [0 1 0] [-1 0 0]])
(def Rz-90 [[0 -1 0] [1 0 0] [0 0 1]])

(defn matpow [m pow]
  (if (zero? pow)
    [[1 0 0] [0 1 0] [0 0 1]]
    (reduce m/inner-product (repeat pow m))))

(def six-rot [R-id Ry-90 (matpow Ry-90 2) (matpow Ry-90 3) Rz-90 (matpow Rz-90 3)])

(defn rotations-around-x [base-rot]
  (map #(m/inner-product (matpow Rx-90 %) base-rot) (range 4)))

(def all-rot (reduce #(concat %1 (rotations-around-x %2)) [] six-rot))

(defn diffs-between-pairs-of-beacons [scanner]
  (let [pairs (for [i (range (count scanner))
                    j (range (count scanner))
                    :when (< i j)]
                [i j])]
    (->> pairs
         (map (fn [[i j]]
                [(set (map #(Math/abs %) (m/sub (nth scanner i) (nth scanner j))))
                 [i j]]))
         (into {}))))

(defn detect-overlapping-pairs-of-beacons [diffs-by-scanner id-a id-b]
  (let [diffs-a (nth diffs-by-scanner id-a)
        diffs-b (nth diffs-by-scanner id-b)]
    (reduce (fn [m [key-a val-a]]
              (let [val-b (get diffs-b key-a)]
                (if (nil? val-b) m (assoc m key-a [val-a val-b]))))
            {}
            diffs-a)))

(defn resolve-identical-beacons [common-pairs]
  (->> common-pairs
       (reduce (fn [m [[a-1 a-2] [b-1 b-2]]]
                 (let [b-set #{b-1 b-2}]
                   (-> m
                       (update a-1 (fnil #(st/intersection % b-set) b-set))
                       (update a-2 (fnil #(st/intersection % b-set) b-set)))))
               {})
       (map #(vector (first %) (first (second %))))))

(defn update-identicals-map [identicals-map identical-beacons id-a id-b]
  (reduce (fn [m [a b]]
            (let [beacon-a [id-a a]
                  beacon-b [id-b b]
                  beacon-id (or (get m beacon-a) (get m beacon-b))]
              (if (nil? beacon-id)
                (let [new-beacon-id (UUID/randomUUID)]
                  (-> m (assoc beacon-a new-beacon-id) (assoc beacon-b new-beacon-id)))
                (-> m (assoc beacon-a beacon-id) (assoc beacon-b beacon-id)))))
          identicals-map
          identical-beacons))

(defn resolve-orientation [b1-source b2-source b1-target b2-target]
  (assert (every? (comp not zero?) (m/sub b2-source b1-source)))
  (loop [[rot & rest] all-rot]
    (if (= (m/sub (m/inner-product rot b2-target) (m/inner-product rot b1-target))
           (m/sub b2-source b1-source))
      rot
      (recur rest))))

(defn resolve-scanner-coords [rot known-beacon tbd-beacon]
  (let [rotated-tbd-beacon (m/inner-product rot tbd-beacon)]
    (m/sub known-beacon rotated-tbd-beacon)))

(defn handle-pair [diffs-by-scanner scanners identicals-map
                   transforms-map id-a id-b]
  (let [common-pairs (detect-overlapping-pairs-of-beacons diffs-by-scanner id-a id-b)]
    (if (>= (count common-pairs) 66) ; n * (n - 1) / 2 with n = 12
      (let [identical-beacons (resolve-identical-beacons (vals common-pairs))
            [[b1-a b1-b] [b2-a b2-b]] (take 2 identical-beacons)
            b1-a (util/get-in-matrix scanners id-a b1-a)
            b2-a (util/get-in-matrix scanners id-a b2-a)
            b1-b (util/get-in-matrix scanners id-b b1-b)
            b2-b (util/get-in-matrix scanners id-b b2-b)
            rot (resolve-orientation b1-a b2-a b1-b b2-b)
            relative-pos (resolve-scanner-coords rot b1-a b1-b)]
        [(update-identicals-map
          identicals-map identical-beacons id-a id-b)
         (update transforms-map id-a conj [id-b rot relative-pos])])
      [identicals-map transforms-map])))

(defn build-identicals-and-transforms [diffs-by-scanner scanners]
  (let [n (count scanners)
        pairs (for [x (range n) y (range n)] [x y])]
    (reduce #(handle-pair diffs-by-scanner scanners (first %1) (second %1)
                          (first %2) (second %2))
            [{} {}] pairs)))

(defn invert-identicals-map [identicals-map]
  (reduce (fn [inverted-m [beacon beacon-id]]
            (update inverted-m beacon-id conj beacon))
          {} identicals-map))

(defn count-duplicates [inverted-identicals-map]
  (- (reduce #(+ %1 (count (second %2))) 0 inverted-identicals-map)
     (count inverted-identicals-map)))


(defn resolve-scanner-positions [transforms-map]
  (loop [combined-rot-and-pos {0 [R-id [0 0 0]]}
         to-visit (map #(cons 0 %) (get transforms-map 0))
         i 0]
    (if (> i 100)
      (println "error")
      (if (empty? to-visit)
        combined-rot-and-pos
        (let [[prev-s tbd-s relative-rot relative-pos] (first to-visit)
              [prev-combined-rot prev-pos] (get combined-rot-and-pos prev-s)
              real-pos (m/add prev-pos (m/inner-product prev-combined-rot relative-pos))
              combined-rot (m/inner-product prev-combined-rot relative-rot)
              new-combined-rot-and-pos (assoc combined-rot-and-pos
                                              tbd-s [combined-rot real-pos])]
          (recur new-combined-rot-and-pos
                 (filter #(not (contains? new-combined-rot-and-pos (second %)))
                         (concat to-visit (map #(cons tbd-s %) (get transforms-map tbd-s))))
                 (inc i)))))))

(defn manhattan [a b]
  (reduce + 0 (map #(Math/abs (m/sub %1 %2)) a b)))

(defn compute-manhattan-distances [scanner-positions]
  (println scanner-positions)
  (map #(manhattan (first %) (second %))
       (for [i (range (count scanner-positions))
             j (range (count scanner-positions))
             :when (< i j)]
         [(nth scanner-positions i) (nth scanner-positions j)])))

(defn run [filename]
  (let [input (util/load-input (str "resources/day19/" filename))
        scanners (parse-input input)
        diffs-by-scanner (mapv diffs-between-pairs-of-beacons scanners)
        total-beacons (reduce + 0 (map count scanners))
        [identicals-map transforms-map]
        (build-identicals-and-transforms diffs-by-scanner scanners)
        scanner-positions (resolve-scanner-positions transforms-map)]
    ; Part 1 answer
    (println (- total-beacons (count-duplicates
                               (invert-identicals-map identicals-map))))
    (apply max (compute-manhattan-distances (map #(second (second %)) scanner-positions)))))
