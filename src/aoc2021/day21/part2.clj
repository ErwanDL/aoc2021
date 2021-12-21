(ns aoc2021.day21.part2
  (:require [aoc2021.util :as util]
            [aoc2021.day21.part1 :as part1]
            [clojure.core.matrix :as m]))

(def arrangements {3 1
                   4 3
                   5 6
                   6 7
                   7 6
                   8 3
                   9 1})

(defn swap-if-needed [cache-wins curr-player]
  (if (zero? curr-player) cache-wins (vec (reverse cache-wins))))

(defn play-quantum [players curr-player cache]
  (let [other-player (bit-xor curr-player 1)]
    (if (>= (second (nth players other-player)) 21) ; Checking if the other player just won
      [(assoc [0 0] other-player 1) cache] ; If yes return a win for them
      (let [cache-key [(nth players curr-player) (nth players other-player)]
            cache-val (get cache cache-key)] ; If the other player didn't win, check cache first
        (if (not (nil? cache-val)) ; If cache contains value, return it
          [(swap-if-needed cache-val curr-player) cache]
          (let [[updated-wins updated-cache]
                (reduce (fn [[wins c] [die-throw n-poss]] ; If cache doesn't contain value : compute it and update cache
                          (let [[place score] (nth players curr-player)
                                new-place (part1/move-pawn place die-throw)
                                new-score (+ score new-place)
                                new-players (assoc players curr-player [new-place new-score])
                                [new-wins new-cache] (play-quantum new-players other-player c)]
                            [(m/add wins (m/mul n-poss new-wins)) new-cache]))
                        [[0 0] cache]
                        arrangements)
                re-updated-cache (assoc updated-cache cache-key (swap-if-needed updated-wins curr-player))]
            [updated-wins re-updated-cache]))))))

(defn run [filename]
  (let [input (util/load-input (str "resources/day21/" filename))
        [p0 p1] (part1/parse-input input)]
    (first (play-quantum [[p0 0] [p1 0]] 0 {}))))