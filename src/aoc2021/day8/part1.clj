(ns aoc2021.day8.part1
  (:require
   [aoc2021.util :as util]
   [clojure.string :as str]))

(defn parse-input [line]
  (let [[patterns outputs] (str/split line #" \| ")]
    [(str/split patterns #" ")  (str/split outputs #" ")]))

(def unique-segment-counts #{2 3 4 7})

(defn count-1-4-7-8 [values]
  (count (filter #(contains? unique-segment-counts %) (map count values))))

(defn run [filename]
  (let [input (util/load-input (str "resources/day8/" filename))
        lines (map parse-input input)]
    (reduce #(+ %1 (count-1-4-7-8 (second %2))) 0 lines)))