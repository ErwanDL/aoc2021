(ns aoc2021.day4.part2
  (:require
   [aoc2021.util :as util]
   [aoc2021.day4.part1 :as part1]))


(defn last-winner [numbers boards]
  (loop [remaining-boards boards
         remaining-numbers numbers]
    (if (= 1 (count remaining-boards))
      (part1/first-winner remaining-numbers remaining-boards)
      (let [updated-boards (map
                            #(part1/update-board % (first remaining-numbers))
                            remaining-boards)]
        (recur (filter (comp not part1/wins?) updated-boards)
               (rest remaining-numbers))))))


(defn run [filename]
  (let [input (util/load-input (str "resources/day4/" filename))
        [numbers boards] (part1/parse-input input)]
    (apply part1/winner-score (last-winner numbers boards))))