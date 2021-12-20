(ns aoc2021.day18.part2
  (:require [aoc2021.util :as util]
            [aoc2021.day18.part1 :as part1]))

(defn max-reducer [prev-max [a b]]
  (max prev-max (part1/magnitude (part1/reduce-nb (part1/add-sf a b)))
       (part1/magnitude (part1/reduce-nb (part1/add-sf b a)))))

(defn run [filename]
  (let [input (util/load-input (str "resources/day18/" filename))
        sf-numbers (map read-string input)]
    (reduce max-reducer 0 (for [a sf-numbers
                                b sf-numbers
                                :when (not= a b)]
                            [a b]))))