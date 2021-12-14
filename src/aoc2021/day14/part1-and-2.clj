(ns aoc2021.day14.part1-and-2
  (:require [aoc2021.util :as util]
            [clojure.string :as s]))

(defn parse-template [template]
  (reduce #(update %1 (s/join %2) (fnil inc 0)) {} (partition 2 1 template)))

(defn parse-rules [rules]
  (reduce #(let [[pair letter] (s/split %2 #" -> ")]
             (assoc %1 pair (first letter))) {} rules))

(defn apply-rule [previous-polymer resulting-polymer rule]
  (let [[pair to-insert] rule
        pair-count (get previous-polymer pair 0)]
    (if (> pair-count 0)
      (-> resulting-polymer
          (update (str (first pair) to-insert) (fnil #(+ pair-count %) 0))
          (update (str to-insert (second pair)) (fnil #(+ pair-count %) 0)))
      resulting-polymer)))

(defn count-letters [polymer first-letter last-letter]
  (->> polymer
       (reduce (fn [count-map [pair count]]
                 (-> count-map
                     (update (first pair) (fnil #(+ count %) 0))
                     (update (second pair) (fnil #(+ count %) 0))))
               {first-letter 1, last-letter 1})
       (reduce #(assoc %1 (first %2) (quot (second %2) 2)) {})))

(defn diff-most-frequent-least-frequent [letter-counts]
  (let [counts (map second letter-counts)
        most-frequent (apply max counts)
        least-frequent (apply min counts)]
    (- most-frequent least-frequent)))

(defn run [filename]
  (let [input (util/load-input (str "resources/day14/" filename))
        template (parse-template (first input))
        rules (parse-rules (nthrest input 2))
        first-letter (first (first input))
        last-letter (last (first input))]
    (loop [i 0
           polymer template]
      (if (= i 40) ; Just change this to 10 for part 1
        (diff-most-frequent-least-frequent
         (count-letters polymer first-letter last-letter))
        (recur (inc i) (reduce #(apply-rule polymer %1 %2) {} rules))))))