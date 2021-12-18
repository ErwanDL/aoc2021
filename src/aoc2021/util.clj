(ns aoc2021.util
  (:require [clojure.string :as str]))


(defn load-input [file-name]
  (str/split-lines (slurp file-name)))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn binary-to-decimal [binary]
  (->> binary
       (reduce (fn [[val exponent] bit]
                 [(+ val (* bit (exp 2 exponent))) (dec exponent)])
               [0 (dec (count binary))])
       (first)))

(defn new-matrix [x y val]
  (vec (repeat x (vec (repeat y val)))))

(defn transpose [m]
  (apply mapv vector m))

(defn get-in-matrix [m x y]
  (nth (nth m x) y))

(defn set-in-matrix [m x y val]
  (assoc m x (assoc (nth m x) y val)))

(def -adjacent-offsets
  [[1 0] [-1 0] [0 1] [0 -1]])

(def -diagonal-offsets
  [[1 1] [1 -1] [-1 1] [-1 -1]])

(defn add-coords [a b]
  [(+ (first a) (first b)) (+ (second a) (second b))])

(defn adjacents [point include-diagonals?]
  (let [offsets (if include-diagonals?
                  (concat -adjacent-offsets -diagonal-offsets)
                  -adjacent-offsets)]
    (map #(add-coords point %) offsets)))