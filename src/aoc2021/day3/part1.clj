(ns aoc2021.day3.part1
  (:require
   [aoc2021.util :as util]))


(defn parse-input [input]
  (map #(Integer/parseInt (str %)) input))

(defn inc-if-1 [bit occurence]
  (if (= 1 bit)
    (inc occurence)
    occurence))

(defn update-ones [ones next-bits]
  (map inc-if-1 next-bits ones))

(defn compute-ones [inputs]
  (reduce update-ones (repeat (count inputs) 0) inputs))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn binary-to-decimal [binary]
  (->> binary
       (reduce (fn [[val exponent] bit]
                 [(+ val (* bit (exp 2 exponent))) (dec exponent)])
               [0 (dec (count binary))])
       (first)))

(defn compute-gamma-and-epsilon [ones n-lines]
  (let [gamma (map #(if (> % (- n-lines %)) 1 0) ones)
        gamma (binary-to-decimal gamma)]
    [gamma (dec (- (exp 2 (count ones)) gamma))]))

(defn run [filename]
  (let [input (util/load-input (str "resources/day3/" filename))
        input (map parse-input input)
        [gamma epsilon] (-> input
                            (compute-ones)
                            (compute-gamma-and-epsilon (count input)))]
    (* gamma epsilon)))