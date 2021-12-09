(ns aoc2021.day9.part1
  (:require [aoc2021.util :as util]))

(defn parse-row [row]
  (map #(Character/digit % 10) row))

(defn init-diff-map [heightmap]
  (map (fn [row]
         (map #(vector % 0) row)) heightmap))
(defn dec-diff [[height diff]]
  [height (dec diff)])

(defn update-diff [prev curr next]
  (reduce #(if (< (first %1) (first %2))
             (dec-diff %1)
             %1)
          curr
          [prev next]))

(defn update-diffs-in-row [diff-row]
  (let [padded-row (concat '([10 0]) diff-row '([10 0]))]
    (map #(apply update-diff %) (partition 3 1 padded-row))))

(defn build-diff-map [diff-map]
  (->> diff-map
       (map update-diffs-in-row)
       (util/transpose)
       (map update-diffs-in-row)
       (util/transpose)))

(defn run [filename]
  (let [input (util/load-input (str "resources/day9/" filename))
        heightmap (map parse-row input)]
    (->> heightmap
         (init-diff-map)
         (build-diff-map)
         (apply concat)
         (filter #(= -4 (second %)))
         (reduce #(+ %1 (inc (first %2))) 0))))