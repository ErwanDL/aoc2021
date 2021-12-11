(ns aoc2021.day11.part2
  (:require [aoc2021.util :as util]
            [clojure.core.matrix :as matrix]
            [aoc2021.day11.part1 :as part1]))



(defn run [filename]
  (let [input (util/load-input (str "resources/day11/" filename))
        grid (part1/parse-input input)]
    (loop [i 0
           g grid
           n-flashes 0]
      (if (matrix/equals (matrix/zero-array [10 10]) g)
        i
        (let [[new-g new-n-flashes] (part1/simulate-step g n-flashes)]
          (recur (inc i) new-g new-n-flashes))))))