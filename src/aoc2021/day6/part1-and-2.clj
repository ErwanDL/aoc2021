(ns aoc2021.day6.part1-and-2
  (:require
   [aoc2021.util :as util]
   [clojure.string :as str]))

(defn parse-input [input]
  (map #(Integer/parseInt %) (str/split (first input) #",")))

(defn build-initial-timers [fishes]
  (vec (reduce #(update %1 %2 inc) (vec (repeat 9 0)) fishes)))

(defn update-timers [timers timer-value n-fish]
  (let [dec-n-fish #(- % n-fish)
        inc-n-fish #(+ % n-fish)
        timers (update timers timer-value dec-n-fish)] ; decrement timer
    (if (= 0 timer-value)
      (-> timers
          (update 6 inc-n-fish) ; reset fish timer
          (update 8 inc-n-fish)); spawn new fish
      (-> timers
          (update (dec timer-value) inc-n-fish)))))

(defn simulate-day [timers]
  (vec (reduce-kv update-timers timers timers)))

(defn run [filename]
  (let [input (util/load-input (str "resources/day6/" filename))
        fishes (parse-input input)]
    (loop [timers (build-initial-timers fishes)
           day 0]
      (if (= 256 day) ; just change that to 80 for part 1 results
        (reduce + timers)
        (recur (simulate-day timers)
               (inc day))))))
