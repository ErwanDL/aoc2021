(ns aoc2021.day18.part1
  (:require [aoc2021.util :as util]))

(defn add-sf [a b]
  [a b])

(defn find-first-that-explodes
  ([sf-number] (find-first-that-explodes sf-number 0 []))
  ([sf-number depth path]
   (if (integer? sf-number)
     nil
     (let [[left right] sf-number]
       (if (>= depth 4)
         path ;; depth >= 4 means explosion
         (let [res-left (find-first-that-explodes
                         left (inc depth) (conj path 0))]
           (if (nil? res-left)
             (find-first-that-explodes right (inc depth) (conj path 1))
             res-left)))))))


(defn path-to-left-regu [sf-number path-to-explode]
  (let [last-right-branching (reverse (drop-while zero? (reverse path-to-explode)))]
    (if (empty? last-right-branching)
      nil
      (loop [p (conj (vec (drop-last last-right-branching)) 0)]
        (if (integer? (get-in sf-number p))
          p
          (recur (conj p 1)))))))

(defn path-to-right-regu [sf-number path-to-explode]
  (let [last-right-branching (reverse (drop-while (comp not zero?) (reverse path-to-explode)))]
    (if (empty? last-right-branching)
      nil
      (loop [p (conj (vec (drop-last last-right-branching)) 1)]
        (if (integer? (get-in sf-number p))
          p
          (recur (conj p 0)))))))

(defn explode-nb [sf-number]
  (let [to-explode (find-first-that-explodes sf-number)]
    (if (nil? to-explode)
      [sf-number false]
      (let [left-regu (path-to-left-regu sf-number to-explode)
            right-regu (path-to-right-regu sf-number to-explode)
            exploded (get-in sf-number to-explode)
            sf-number (assoc-in sf-number to-explode 0)
            sf-number (if (nil? left-regu)
                        sf-number
                        (update-in sf-number left-regu #(+ (first exploded) %)))

            sf-number (if (nil? right-regu)
                        sf-number
                        (update-in sf-number right-regu #(+ (second exploded) %)))]
        [sf-number true]))))

(defn split-nb [sf-number]
  (if (integer? sf-number)
    (if (>= sf-number 10)
      [[(quot sf-number 2) (- sf-number (quot sf-number 2))] true]
      [sf-number false])
    (let [[left right] sf-number
          [new-left split-left?] (split-nb left)]
      (if split-left?
        [[new-left right] true]
        (let [[new-right split-right?] (split-nb right)]
          [[left new-right] split-right?])))))

(defn reduce-nb [sf-number]
  (let [[after-explosion exploded?] (explode-nb sf-number)]
    (if exploded?
      (recur after-explosion)
      (let [[after-split split?] (split-nb after-explosion)]
        (if split?
          (recur after-split)
          after-split)))))

(defn magnitude [sf-number]
  (if (integer? sf-number)
    sf-number
    (+ (* 3 (magnitude (first sf-number)))
       (* 2 (magnitude (second sf-number))))))

(defn run [filename]
  (let [input (util/load-input (str "resources/day18/" filename))
        sf-numbers (map read-string input)]
    (->> sf-numbers
         (reduce #(reduce-nb (add-sf %1 %2)))
         (magnitude))))