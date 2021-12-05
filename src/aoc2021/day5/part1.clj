(ns aoc2021.day5.part1
  (:require
   [aoc2021.util :as util]))

(defn parse-row [row]
  (->> row
       (re-matches #"([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)")
       (rest)
       (map #(Integer/parseInt %))
       (partition 2)))

(defn is-horiz? [[start end]]
  (= (second start) (second end)))

(defn is-vert? [[start end]]
  (= (first start) (first end)))

(defn add-horiz-line-to-map [map [start end]]
  (let [distance (- (first end) (first start))
        increment (/ distance (Math/abs distance))]
    (loop [i (first start)
           m map]
      (let [updated-map (update m [i (second start)]
                                (fnil inc 0))]
        (if (= (first end) i)
          updated-map
          (recur (+ increment i)
                 updated-map))))))

(defn add-vert-line-to-map [map [start end]]
  (let [distance (- (second end) (second start))
        increment (/ distance (Math/abs distance))]
    (loop [j (second start)
           m map]
      (let [updated-map (update m [(first start) j]
                                (fnil inc 0))]
        (if (= j (second end))
          updated-map
          (recur (+ increment j)
                 updated-map))))))

(defn build-counts-map [vert-lines horiz-lines]
  (let [map-with-horiz-counts (reduce #(add-horiz-line-to-map %1 %2) {} horiz-lines)]
    (reduce #(add-vert-line-to-map %1 %2)
            map-with-horiz-counts
            vert-lines)))


(defn run [filename]
  (let [input (util/load-input (str "resources/day5/" filename))
        lines (map parse-row input)
        vert-lines  (filter is-vert? lines)
        horiz-lines (filter is-horiz? lines)
        counts-map (build-counts-map vert-lines horiz-lines)]
    (count (filter #(> (second %) 1) counts-map))))