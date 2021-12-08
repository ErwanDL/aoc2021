(ns aoc2021.day7.part1
  (:require
   [aoc2021.util :as util]
   [clojure.string :as str]))

(defn parse-input [input]
  (map #(Integer/parseInt %) (str/split input #",")))

(defn median [positions]
  (nth (sort positions) (quot (count positions) 2)))

(defn count-fuel [positions destination]
  (reduce #(+ %1 (Math/abs (- destination %2))) 0 positions))

(defn run [filename]
  (let [input (util/load-input (str "resources/day7/" filename))
        positions (parse-input (first input))]
    (count-fuel positions (median positions))))