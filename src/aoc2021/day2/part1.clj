(ns aoc2021.day2.part1
  (:require [clojure.string :as str]
            [aoc2021.util :as util]))


(defn parse-input [input]
  (map #(vector (first %) (Integer/parseInt (second %)))
       (map #(str/split % #" ") input)))


(defn update-position [[horiz depth] [direction value]]
  (case direction
    "forward" [(+ horiz value) depth]
    "down" [horiz (+ depth value)]
    "up" [horiz (- depth value)]))


(defn compute-position [inputs]
  (reduce update-position [0 0] inputs))

(defn run [filename]
  (let [input (util/load-input (str "resources/day2/" filename))
        input (parse-input input)
        [horiz depth] (compute-position input)]
    (* horiz depth)))