(ns aoc2021.day10.part1
  (:require [aoc2021.util :as util]))

(def matching-brackets
  {\( \), \[ \] \{ \}, \< \>})

(defn matches? [tok last-opening-tok]
  (= tok (get matching-brackets last-opening-tok)))

(defn parse-tokens
  ([next-tokens] (parse-tokens next-tokens []))
  ([next-tokens opened-chunks]
   (if (zero? (count next-tokens))
     opened-chunks
     (let [[tok & rest] next-tokens]
       (if (contains? #{\( \{ \[ \<} tok)
         (recur rest (conj opened-chunks tok))
         (if (matches? tok (peek opened-chunks))
           (recur rest (pop opened-chunks))
           tok))))))

(def illegal-char-points {\) 3, \] 57, \} 1197, \> 25137})

(defn run [filename]
  (let [input (util/load-input (str "resources/day10/" filename))]
    (->> input
         (map parse-tokens)
         (filter #(contains? illegal-char-points %))
         (reduce #(+ %1 (get illegal-char-points %2)) 0))))