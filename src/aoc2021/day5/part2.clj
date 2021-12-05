(ns aoc2021.day5.part2
  (:require
   [aoc2021.util :as util]
   [aoc2021.day5.part1 :as part1]))

(defn compute-increment [distance]
  (if (zero? distance)
    0
    (/ distance (Math/abs distance))))

(defn add-line-to-map [map [start end]]
  (let [horiz-distance (- (first end) (first start))
        horiz-increment (compute-increment horiz-distance)
        vert-distance (- (second end) (second start))
        vert-increment (compute-increment vert-distance)]
    (loop [i (first start)
           j (second start)
           m map]
      (let [updated-map (update m [i j]
                                (fnil inc 0))]
        (if (and (= i (first end)) (= j (second end)))
          updated-map
          (recur (+ horiz-increment i)
                 (+ vert-increment j)
                 updated-map))))))

(defn build-counts-map [lines]
  (reduce #(add-line-to-map %1 %2)
          {}
          lines))


(defn run [filename]
  (let [input (util/load-input (str "resources/day5/" filename))
        lines (map part1/parse-row input)
        counts-map (build-counts-map lines)]
    (count (filter #(> (second %) 1) counts-map))))