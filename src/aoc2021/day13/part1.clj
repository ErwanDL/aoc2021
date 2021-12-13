(ns aoc2021.day13.part1
  (:require [aoc2021.util :as util]
            [clojure.string :as str]))

(defn parse-fold-instr [fold-instr]
  (let [matches (re-matches #"fold along (x|y)=([0-9]+)" fold-instr)]
    [(second matches) (Integer/parseInt (nth matches 2))]))

(defn parse-dot [dot]
  (mapv #(Integer/parseInt %) (str/split dot #",")))

(defn parse-input [input]
  (reduce (fn [[dots fold-instrs] line]
            (cond
              (= "" line) [dots fold-instrs]
              (str/starts-with? line "fold along") [dots
                                                    (conj fold-instrs
                                                          (parse-fold-instr line))]
              :else [(conj dots (parse-dot line)) fold-instrs]))
          [[] []]
          input))

(defn apply-fold-to-coord [coord fold-value]
  (if (> coord fold-value)
    (- fold-value (- coord fold-value))
    coord))

(defn apply-fold-to-dot [dot fold-instr]
  (case (first fold-instr)
    "x" [(apply-fold-to-coord (first dot) (second fold-instr)) (second dot)]
    "y" [(first dot) (apply-fold-to-coord (second dot) (second fold-instr))]))

(defn run [filename]
  (let [input (util/load-input (str "resources/day13/" filename))
        [dots fold-instrs] (parse-input input)]
    (->> dots
         (map #(apply-fold-to-dot % (first fold-instrs)))
         (set)
         (count))))