(ns aoc2021.day17.part1
  (:require [aoc2021.util :as util]))

;; This day is rather fun and mathematical!
;; Part 1 can be done with just a pen and a paper,
;; but let's automatise it anyway.
;;
;; First of all, let's remark that we don't really care about
;; the initial horizontal velocity: with the given examples,
;; it seems that we can assume that there will always be an
;; initial horizontal velocity that gets to 0 (because of
;; drag) within the horizontal range of the target area.
;;
;; So let's only focus on vertical movement: we remark that
;; ascension and fall are symmetrical, ie if we shoot upwards
;; with initial velocity V, the probe will eventually come back down
;; passing by the exact same intermediate heights that it passed by
;; on its way up. Also, we remark that when it reaches the 0 point on
;; its way down, it will have `- V - 1` vertical velocity.
;; Therefore, if we call Y_min the lowest height of the target area,
;; the highest value of V that still ends up in the target area is :
;; - V - 1 = Y_min ==> V = - Y_min - 1
;;
;; Lastly, we compute that the highest point reached is:
;; sum(i for i in [1; V]) = V * (V + 1) / 2

(defn parse-target-area [line]
  (let [vals (re-matches
              #"target area: x=([0-9]+)..([0-9]+), y=(-[0-9]+)..(-[0-9]+)"
              line)]
    (map #(Integer/parseInt %) (rest vals))))

(defn highest-y [min-y]
  (let [v_init (dec (- min-y))]
    (quot (* v_init (inc v_init)) 2)))

(defn run [filename]
  (let [input (util/load-input (str "resources/day17/" filename))
        [_min-x _max-x min-y _max-y] (parse-target-area (first input))]
    (highest-y min-y)))