(ns aoc2021.day1
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2021.day1.part1 :as part1]
            [aoc2021.day1.part2 :as part2]))

(deftest test-inputs
  (testing "Day 1, part 1"
    (is (= 7 (part1/run "test.txt")))
    (is (= 1688 (part1/run "input.txt"))))
  (testing "Day 1, part 2"
    (is (= 5 (part2/run "test.txt")))
    (is (= 1728 (part2/run "input.txt")))))
