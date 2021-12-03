(ns aoc2021.day1.part1
  (:require [aoc2021.util :as util]))


(defn inc-if-greater [[counter prev-val] next-val]
  (if (> next-val prev-val)
    [(inc counter) next-val]
    [counter next-val]))

(defn parse-input [input]
  (map #(Integer/parseInt %) input))

(defn run [filename]
  (let [input (util/load-input (str "resources/day1/" filename))
        input (parse-input input)]
    (first (reduce inc-if-greater [0 (first input)] (rest input)))))