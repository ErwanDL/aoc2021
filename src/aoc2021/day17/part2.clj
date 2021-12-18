(ns aoc2021.day17.part2
  (:require [aoc2021.util :as util]
            [aoc2021.day17.part1 :as part1]))

(defn reaches-target-area? [[v-x-init v-y-init] min-x max-x min-y max-y]
  (loop [v-x v-x-init
         v-y v-y-init
         x 0
         y 0]
    (if (or (> x max-x)
            (and (< x min-x) (zero? v-x))
            (< y min-y))
      false
      (if (and (<= x max-x) (>= x min-x) (<= y max-y) (>= y min-y))
        true
        (recur (max 0 (dec v-x))
               (dec v-y)
               (+ x v-x)
               (+ y v-y))))))

(defn run [filename]
  (let [input (util/load-input (str "resources/day17/" filename))
        [min-x max-x min-y max-y] (part1/parse-target-area (first input))
        max-v-y-init (dec (- min-y))]
    (count (filter #(reaches-target-area? % min-x max-x min-y max-y)
                   (for [x (range (inc max-x)) y (range min-y (inc max-v-y-init))] [x y])))))