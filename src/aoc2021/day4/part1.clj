(ns aoc2021.day4.part1
  (:require
   [aoc2021.util :as util]
   [clojure.string :as str]))

(defn parse-numbers [numbers]
  (map #(Integer/parseInt %) (str/split numbers #",")))

(defn parse-board-row [row]
  (map #(vector (Integer/parseInt %) false) (str/split (str/triml row) #"\s+")))

(defn parse-board [board]
  (map parse-board-row board))

(defn parse-input [input]
  (let [numbers (first input)
        boards (->> (nthrest input 2)
                    (partition 5 6)
                    (map parse-board))]
    [(parse-numbers numbers) boards]))

(defn update-board [board drawn]
  (for [line board]
    (map (fn [[val marked]]
           (if (= drawn val) [val true] [val marked])) line)))


(defn complete? [row]
  (every? #(true? (second %)) row))

(defn wins? [board]
  (or (some true? (map complete? board))
      (some true? (map complete? (util/transpose board)))))

(defn first-winner [numbers boards]
  (loop [marked-boards boards
         remaining-numbers numbers
         last-drawn nil]
    (if-let [winner (first (filter wins? marked-boards))]
      [winner last-drawn]
      (recur (map #(update-board % (first remaining-numbers)) marked-boards)
             (rest remaining-numbers)
             (first remaining-numbers)))))

(defn winner-score [winner-board last-drawn]
  (->> winner-board
       (apply concat)
       (reduce (fn [sum [val marked]]
                 (if marked
                   sum
                   (+ sum val))) 0)
       (* last-drawn)))

(defn run [filename]
  (let [input (util/load-input (str "resources/day4/" filename))
        [numbers boards] (parse-input input)]
    (apply winner-score (first-winner numbers boards))))