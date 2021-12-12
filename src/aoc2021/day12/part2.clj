(ns aoc2021.day12.part2
  (:require [aoc2021.util :as util]
            [aoc2021.day12.part1 :as part1]))

(defn possible-paths [current-cave graph visited]
  (let [next-caves (filter #(not= "start" %) (get graph current-cave))
        visited-small-caves (filter part1/small-cave? visited)
        paths-with-or-without-joker (group-by #(.contains visited-small-caves %) next-caves)]
    [(get paths-with-or-without-joker false) (get paths-with-or-without-joker true)]))


(defn explore-v2 [current-path paths has-joker graph]
  (let [current-cave (last current-path)]
    (if (= "end" current-cave)
      (conj paths current-path)
      (let [[next-caves-no-joker next-caves-using-joker] (possible-paths current-cave graph current-path)
            paths-no-joker (reduce
                            #(explore-v2 (conj current-path %2) %1 has-joker graph)
                            paths next-caves-no-joker)]
        (if has-joker
          (reduce
           #(explore-v2 (conj current-path %2) %1 false graph)
           paths-no-joker next-caves-using-joker)
          paths-no-joker)))))

(defn run [filename]
  (let [input (util/load-input (str "resources/day12/" filename))
        graph (part1/parse-input input)]
    (count (explore-v2 ["start"] [] true graph))))