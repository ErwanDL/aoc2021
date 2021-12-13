(ns aoc2021.day13.part2
  (:require [aoc2021.util :as util]
            [clojure.string :as str]
            [aoc2021.day13.part1 :as part1]))

(defn apply-all-fold-instrs [fold-instrs dots]
  (reduce (fn [prev-dots fold-instr]
            (set (map #(part1/apply-fold-to-dot % fold-instr) prev-dots)))
          dots
          fold-instrs))

(defn get-dimensions [dots]
  [(inc (apply max (map second dots))) (inc (apply max (map first dots)))])

(defn empty-matrix [x y default-value]
  (vec (repeat x (vec (repeat y default-value)))))

(defn set-in-matrix [m x y val]
  (assoc m x (assoc (nth m x) y val)))

(defn dots-to-string [dots]
  (let [[x y] (get-dimensions dots)
        m (empty-matrix x y \.)
        char-matrix (reduce #(set-in-matrix %1 (second %2) (first %2) \#) m dots)]
    (str/join "\n" (map str/join char-matrix))))

(defn run [filename]
  (let [input (util/load-input (str "resources/day13/" filename))
        [dots fold-instrs] (part1/parse-input input)]
    (->> dots
         (apply-all-fold-instrs fold-instrs)
         (dots-to-string)
         (spit (str "resources/day13/result-" filename)))))