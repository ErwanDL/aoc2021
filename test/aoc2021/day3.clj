(ns aoc2021.day3
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2021.day3.part1 :as part1]
            [aoc2021.day3.part2 :as part2]))

(deftest test-day2
  (testing "Day 3, part 1"
    (is (= 198 (part1/run "test.txt")))
    (is (= 852500 (part1/run "input.txt"))))
  (testing "Day 3, part 2"
    (is (= 230 (part2/run "test.txt")))
    (is (= 1007985 (part2/run "input.txt")))))
