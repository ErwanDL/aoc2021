(ns aoc2021.day12.part1
  (:require [aoc2021.util :as util]
            [clojure.string :as str]))

(defn parse-input [input]
  (->> input
       (map #(str/split % #"-"))
       (reduce #(-> %1 (update (first %2) (fnil conj []) (second %2))
                    (update (second %2) (fnil conj []) (first %2))) {})))

(defn small-cave? [cave-name]
  (Character/isLowerCase (first cave-name)))

(defn next-unvisited [next-caves visited]
  (let [visited-small-caves (filter small-cave? visited)]
    (filter #(not (.contains visited-small-caves %)) next-caves)))

(defn explore [current-path paths graph]
  (let [current-cave (last current-path)]
    (if (= "end" current-cave)
      (conj paths current-path)
      (let [next-caves (next-unvisited (get graph current-cave) current-path)]
        (reduce #(explore (conj current-path %2) %1 graph) paths next-caves)))))

(defn run [filename]
  (let [input (util/load-input (str "resources/day12/" filename))
        graph (parse-input input)]
    (count (explore ["start"] [] graph))))