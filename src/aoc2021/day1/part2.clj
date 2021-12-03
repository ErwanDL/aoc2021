(ns aoc2021.day1.part2
  (:require [aoc2021.util :as util]
            [aoc2021.day1.part1 :as part1]))


(defn convert-to-sums [depths]
  (reduce (fn [[first second sums] third]
            (let [sum (conj sums (+ first second third))]
              [second third sum]))
          [(first depths) (second depths) []]
          (nthrest depths 2)))

(defn run [filename]
  (let [input (util/load-input (str "resources/day1/" filename))
        input (part1/parse-input input)
        sums (last (convert-to-sums input))]
    (first (reduce part1/inc-if-greater [0 (first sums)] (rest sums)))))