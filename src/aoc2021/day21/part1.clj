(ns aoc2021.day21.part1
  (:require [aoc2021.util :as util]))


(defn parse-line [line]
  (Integer/parseInt (apply str (nthrest line 28))))

(defn parse-input [input]
  [(parse-line (first input)) (parse-line (second input))])

(defn move-pawn [current-space amount]
  (let [s (+ current-space amount)
        r (rem s 10)]
    (if (zero? r)
      10
      r)))

(defn roll-die [die]
  (let [r (rem (inc die) 100)]
    (if (zero? r)
      100
      r)))

(defn play-turn [score space die]
  (let [[new-die sum] (loop [d die, s 0, i 0]
                        (if (>= i 3) [d s]
                            (recur (roll-die d) (+ s (roll-die d)) (inc i))))
        new-space (move-pawn space sum)]
    [(+ score new-space) new-space new-die]))

(defn play-deterministic [p0 p1]
  (let [player0 [0 p0], player1 [0 p1]]
    (loop [players [player0 player1]
           curr-player 0
           die 100
           n-rolls 0]
      (let [[score space] (nth players curr-player)
            [new-score new-space new-die] (play-turn score space die)
            new-n-rolls (+ 3 n-rolls)
            other-player (bit-xor curr-player 1)]
        (if (>= new-score 1000)
          (* new-n-rolls (first (nth players other-player)))
          (recur (assoc players curr-player [new-score new-space])
                 other-player
                 new-die
                 new-n-rolls))))))

(defn run [filename]
  (let [input (util/load-input (str "resources/day21/" filename))
        [p0 p1] (parse-input input)]
    (play-deterministic p0 p1)))