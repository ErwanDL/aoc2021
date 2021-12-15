(ns aoc2021.day15.part2
  (:require [aoc2021.day15.part1 :as part1]
            [aoc2021.util :as util]))

(defn inc-or-wrap [val]
  (if (= 9 val)
    1
    (inc val)))

(defn concat-horiz [grid-a grid-b]
  (mapv #(vec (concat %1 %2)) grid-a grid-b))

(defn inc-grid [grid]
  (mapv #(mapv inc-or-wrap %) grid))

(defn super-line [grid]
  (loop [i 0
         line grid
         prev-grid grid]
    (if (= 4 i)
      line
      (recur (inc i) (concat-horiz line (inc-grid prev-grid))
             (inc-grid prev-grid)))))

(defn super-grid [line]
  (loop [i 0
         super-grid line
         prev-line line]
    (if (= 4 i)
      super-grid
      (recur (inc i) (concat super-grid (inc-grid prev-line))
             (inc-grid prev-line)))))


(defn run [filename]
  (let [input (util/load-input (str "resources/day15/" filename))
        grid (part1/parse-grid input)
        super-grid (super-grid (super-line grid))]
    (part1/dijkstra super-grid)))