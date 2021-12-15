(ns aoc2021.day15.part1
  (:require [aoc2021.util :as util]))

(defn parse-grid [input]
  (mapv (fn [row]
          (mapv #(Character/digit % 10) row)) input))

(defn init-distances [n-rows n-cols]
  (into {} (for [x (range n-rows)
                 y (range n-cols)]
             [[x y] Integer/MAX_VALUE])))

;; Inspired by https://stackoverflow.com/a/671779/12153266
(defn make-pqueue []
  (sorted-set))

(defn pqueue-add [pq x prio]
  (conj pq [prio x]))

(defn pqueue-pop [pq]
  (let [top (first pq)]
    [(disj pq top) (second top)]))

(defn valid-adjacent [pos n-rows n-cols visited]
  (filter #(and (>= (first %) 0) (>= (second %) 0)
                (< (first %) n-rows) (< (second %) n-cols)
                (not (contains? visited %)))
          (util/adjacents pos false)))

(defn update-distance [distances pq to-update current grid]
  (let [vertex-length (util/get-in-matrix grid (first to-update) (second to-update))
        new-dist (+ (get distances current) vertex-length)
        best-dist (min new-dist (get distances to-update))]
    [(assoc distances to-update best-dist) (pqueue-add pq to-update best-dist)]))


(defn dijkstra [grid]
  (let [n-rows (count grid)
        n-cols (count (first grid))
        end [(dec n-rows) (dec n-cols)]
        distances (init-distances n-rows n-cols)]
    (loop [distances (assoc distances [0 0] 0)
           pq (make-pqueue)
           visited #{}
           curr [0 0]
           sec 1000000000]
      (if (= 0 sec)
        nil
        (if (= end curr)
          (get distances curr)
          (let [visited (conj visited curr)
                adj (valid-adjacent curr n-rows n-cols visited)
                [distances pq] (reduce #(update-distance
                                         (first %1) (second %1) %2 curr grid)
                                       [distances pq] adj)
                [pq next] (pqueue-pop pq)]
            (recur distances pq visited next (dec sec))))))))


(defn run [filename]
  (let [input (util/load-input (str "resources/day15/" filename))
        grid (parse-grid input)]
    (dijkstra grid)))