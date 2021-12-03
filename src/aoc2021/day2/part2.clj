(ns aoc2021.day2.part2
  (:require [aoc2021.util :as util]
            [aoc2021.day2.part1 :as part1]))



(defn update-position [[horiz depth aim] [direction value]]
  (case direction
    "forward" [(+ horiz value) (+ depth (* aim value)) aim]
    "down" [horiz depth (+ aim value)]
    "up" [horiz depth (- aim value)]))


(defn compute-final-position [inputs]
  (reduce update-position [0 0 0] inputs))

(defn run [filename]
  (let [input (util/load-input (str "resources/day2/" filename))
        input (part1/parse-input input)
        [horiz depth _] (compute-final-position input)]
    (* horiz depth)))