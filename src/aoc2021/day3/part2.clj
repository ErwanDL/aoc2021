(ns aoc2021.day3.part2
  (:require
   [aoc2021.util :as util]
   [aoc2021.day3.part1 :as part1]))


(defn find-most-common [numbers pos]
  (let [ones-count (reduce #(if (= 1 (nth %2 pos)) (inc %1) %1) 0 numbers)]
    (if (>= ones-count (- (count numbers) ones-count))
      1
      0)))

(defn find-least-common [numbers pos]
  (let [zeros-count (reduce #(if (= 0 (nth %2 pos)) (inc %1) %1) 0 numbers)]
    (if (<= zeros-count (- (count numbers) zeros-count))
      0
      1)))

(defn filter-by-bit-criteria [remaining-numbers bit-criteria pos]
  (filter #(= bit-criteria (nth % pos)) remaining-numbers))

(defn find-rating [input bit-criteria-fn]
  (loop [pos 0
         remaining-numbers input]
    (if (= 1 (count remaining-numbers))
      (first remaining-numbers)
      (let [bit-criteria (bit-criteria-fn remaining-numbers pos)]
        (recur (inc pos) (filter-by-bit-criteria
                          remaining-numbers
                          bit-criteria pos))))))

(defn run [filename]
  (let [input (util/load-input (str "resources/day3/" filename))
        input (map part1/parse-input input)
        oxygen-rating (find-rating input find-most-common)
        co2-rating (find-rating input find-least-common)]
    (* (part1/binary-to-decimal oxygen-rating)
       (part1/binary-to-decimal co2-rating))))