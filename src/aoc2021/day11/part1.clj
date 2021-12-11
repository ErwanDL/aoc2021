(ns aoc2021.day11.part1
  (:require [aoc2021.util :as util]
            [clojure.core.matrix :as matrix]
            clojure.set))

(def grid-size 10)

(defn parse-input [input]
  (mapv (fn [row]
          (mapv #(Character/digit % 10) row)) input))

(def offsets
  [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])

(defn is-in-grid? [[x y]]
  (and (>= x 0) (< x grid-size) (>= y 0) (< y grid-size)))

(defn inc-if-not-flashed [val]
  (if (> val 0) (inc val) val))

(defn adjacents [pos]
  (reduce #(if (is-in-grid? (matrix/add pos %2))
             (conj %1 (matrix/add pos %2))
             %1) [] offsets))

(defn should-flash? [[x y] grid]
  (= (matrix/mget grid x y) 10))

(defn flash [[x y] grid]
  (let [updated-grid (matrix/mset grid x y 0)
        updated-grid (reduce
                      #(update-in %1 %2 inc-if-not-flashed)
                      updated-grid (adjacents [x y]))]
    [updated-grid (set (filter #(should-flash? % updated-grid) (adjacents [x y])))]))

(defn find-in-row-that-should-flash [row row-index]
  (reduce-kv #(if (= %3 10)
                (conj %1 [row-index %2])
                %1) [] row))

(defn find-that-should-flash [grid]
  (reduce-kv #(concat (find-in-row-that-should-flash %3 %2) %1) [] grid))

(defn simulate-step [grid n-flashes]
  (loop [g (matrix/emap inc grid)
         next-to-flash (set (find-that-should-flash g))
         n-flashes n-flashes]
    (if (empty? next-to-flash)
      [g n-flashes]
      (let [next-pos (first next-to-flash)
            rest-to-flash (disj next-to-flash next-pos)
            [updated-grid new-to-flash] (flash next-pos g)]
        (recur updated-grid
               (clojure.set/union rest-to-flash new-to-flash)
               (inc n-flashes))))))

(defn run [filename]
  (let [input (util/load-input (str "resources/day11/" filename))
        grid (parse-input input)]
    (loop [i 100
           g grid
           n-flashes 0]
      (if (zero? i)
        n-flashes
        (let [[new-g new-n-flashes] (simulate-step g n-flashes)]
          (recur (dec i) new-g new-n-flashes))))))