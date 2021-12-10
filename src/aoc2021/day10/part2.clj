(ns aoc2021.day10.part2
  (:require [aoc2021.day10.part1 :as part1]
            [aoc2021.util :as util]))

(def completion-char-points {\( 1, \[ 2, \{ 3, \< 4})

(defn score [opened-chunks]
  (reduce #(+ (* 5 %1) (get completion-char-points %2)) 0 (reverse opened-chunks)))

(defn winner [scores]
  (nth (sort scores) (quot (count scores) 2)))

(defn run [filename]
  (let [input (util/load-input (str "resources/day10/" filename))]
    (->> input
         (map part1/parse-tokens)
         (filter #(not (contains? part1/illegal-char-points %)))
         (map score)
         (winner))))