(ns aoc2021.day7.part2
  (:require
   [aoc2021.util :as util]
   [aoc2021.day7.part1 :as part1]))


(defn mean [positions]
  (double (/ (reduce + positions) (count positions))))

(defn fuel-quadratic [pos dest]
  (let [n (Math/abs (- dest pos))]
    (/ (* n (inc n)) 2)))

(defn total-fuel [positions destination]
  (reduce #(+ %1 (fuel-quadratic %2 destination)) 0 positions))

(defn run [filename]
  (let [input (util/load-input (str "resources/day7/" filename))
        positions (part1/parse-input (first input))
        mean-pos (mean positions)]
    (Math/min (total-fuel positions (int (Math/floor mean-pos)))
              (total-fuel positions (int (Math/ceil mean-pos))))))