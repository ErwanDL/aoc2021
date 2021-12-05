(ns aoc2021.day4
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2021.day4.part1 :as part1]
            [aoc2021.day4.part2 :as part2]))

(deftest test-inputs
  (testing "Day 4, part 1"
    (is (= 4512 (part1/run "test.txt")))
    (is (= 11774 (part1/run "input.txt"))))
  (testing "Day 4, part 2"
    (is (= 1924 (part2/run "test.txt")))
    (is (= 4495 (part2/run "input.txt")))))
