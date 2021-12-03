(ns aoc2021.day2
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2021.day2.part1 :as part1]
            [aoc2021.day2.part2 :as part2]))

(deftest test-day2
  (testing "Day 2, part 1"
    (is (= 150 (part1/run "test.txt")))
    (is (= 1868935 (part1/run "input.txt"))))
  (testing "Day 2, part 2"
    (is (= 900 (part2/run "test.txt")))
    (is (= 1965970888 (part2/run "input.txt")))))
