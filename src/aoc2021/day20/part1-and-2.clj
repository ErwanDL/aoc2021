(ns aoc2021.day20.part1-and-2
  (:require [aoc2021.util :as util]
            [clojure.core.matrix :as matrix]))

(defn parse-char [c]
  (if (= \# c) 1 0))

(defn parse-enhancement-alg [line]
  (mapv parse-char line))

(defn parse-img [lines]
  (mapv #(mapv parse-char %) lines))

(defn pad-twice [m val]
  (let [padded-rows (mapv #(vec (concat [val val] % [val val])) m)
        n (count (first padded-rows))
        row (vec (repeat n val))]
    (vec (concat [row row] padded-rows [row row]))))

(defn window-value [m i j]
  (let [pixels (for [x (range (dec i) (+ 2 i))
                     y (range (dec j) (+ 2 j))]
                 [x y])]
    (util/binary-to-decimal
     (map #(util/get-in-matrix m (first %) (second %)) pixels))))

(defn enhance-img [img enhancement-alg padding-val]
  (let [n (count img)
        new-n (+ 2 n)
        padded-img (pad-twice img padding-val)
        out-img (util/new-matrix new-n new-n 0)
        coords (for [x (range new-n) y (range new-n)] [x y])]
    [(reduce (fn [out-img [x y]]
               (let [index (window-value padded-img (inc x) (inc y))]
                 (util/set-in-matrix out-img x y (nth enhancement-alg index))))
             out-img
             coords)
     (if (zero? padding-val)
       (first enhancement-alg)
       (last enhancement-alg))]))

(defn run [filename]
  (let [input (util/load-input (str "resources/day20/" filename))
        enhancement-alg (parse-enhancement-alg (first input))
        input-img (parse-img (nthrest input 2))
        enhanced (loop [i 0
                        in-img input-img
                        padding-val 0]
                   (if (>= i 50) ; change to 2 for part 1
                     in-img
                     (let [[enhanced-img next-padding-val]
                           (enhance-img in-img enhancement-alg padding-val)]
                       (recur (inc i) enhanced-img next-padding-val))))]
    (matrix/esum enhanced)))
