(ns aoc2021.day9.part2
  (:require [aoc2021.util :as util]
            [aoc2021.day9.part1 :as part1]
            clojure.set))

(defn add-to-basin? [[x y] basin heightmap]
  (and (>= x 0)
       (>= y 0)
       (< x (count heightmap))
       (< y (count (first heightmap)))
       (not= 9 (nth (nth heightmap x) y))
       (not (contains? basin [x y]))))

(defn expand-basin [basin [x y] heightmap]
  (let [basin (conj basin [x y])
        next-pos [[(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)]]]
    (reduce #(if (add-to-basin? %2 %1 heightmap)
               (expand-basin %1 %2 heightmap)
               %1) basin next-pos)))

(defn add-pos-to-diff-map [diff-map]
  (map-indexed (fn [i row]
                 (map-indexed #(conj %2 [i %1]) row)) diff-map))

(defn get-low-points [diff-map-with-pos]
  (filter #(= -4 (second %)) (apply concat diff-map-with-pos)))

(defn run [filename]
  (let [input (util/load-input (str "resources/day9/" filename))
        heightmap (map part1/parse-row input)]
    (->> heightmap
         (part1/init-diff-map)
         (part1/build-diff-map)
         (add-pos-to-diff-map)
         (get-low-points)
         (map #(expand-basin #{} (nth % 2) heightmap))
         (map count)
         (sort >)
         (take 3)
         (apply *))))