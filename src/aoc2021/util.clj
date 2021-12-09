(ns aoc2021.util
  (:require [clojure.string :as str]))


(defn load-input [file-name]
  (str/split-lines (slurp file-name)))


(defn transpose [m]
  (apply mapv vector m))