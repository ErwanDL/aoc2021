(ns aoc2021.day16.part2
  (:require [aoc2021.util :as util]
            [aoc2021.day16.part1 :as part1]))

(defn parse-operator-packet [data
                             initial-length remaining update-remaining-fn
                             initial-value update-value-fn
                             parse-packet-fn]
  (loop [remaining remaining
         accumulated-length initial-length
         value initial-value
         tail data]
    (if (zero? remaining)
      [{:value value
        :length accumulated-length} tail]
      (let [[packet-info tail] (parse-packet-fn tail)]
        (recur (update-remaining-fn remaining packet-info)
               (+ accumulated-length (get packet-info :length))
               (update-value-fn value (get packet-info :value))
               tail)))))

(defn parse-zero-ltid [data initial-value update-value-fn parse-packet-fn]
  (let [[subpackets-length tail] (part1/parse-subpackets-length data)]
    (parse-operator-packet tail
                           22 subpackets-length #(- %1 (get %2 :length))
                           initial-value update-value-fn
                           parse-packet-fn)))

(defn parse-one-ltid [data initial-value update-value-fn parse-packet-fn]
  (let [[subpacket-count tail] (part1/parse-subpacket-count data)]
    (parse-operator-packet tail
                           18 subpacket-count (fn [remaining _] (dec remaining))
                           initial-value update-value-fn
                           parse-packet-fn)))

(defn gt [current other]
  (if (nil? current)
    other
    (if (> current other) 1 0)))

(defn lt [current other]
  (if (nil? current)
    other
    (if (< current other) 1 0)))

(defn eq [current other]
  (if (nil? current)
    other
    (if (= current other) 1 0)))

(defn interpret-type-id [type-id]
  (case type-id
    0 [0 +]
    1 [1 *]
    2 [Long/MAX_VALUE min]
    3 [0 max]
    5 [nil gt]
    6 [nil lt]
    7 [nil eq]))

(defn parse-packet [data]
  (let [[_version type-id tail] (part1/parse-packet-header data)]
    (if (= 4 type-id)
      (let [[packet-info tail] (part1/parse-literal-packet tail)]
        [packet-info tail])
      (let [[length-type-id & tail] tail
            ltid-variant (if (zero? length-type-id)
                           parse-zero-ltid
                           parse-one-ltid)
            [initial-val update-val-fn] (interpret-type-id type-id)]
        (ltid-variant tail initial-val update-val-fn parse-packet)))))

(defn run [filename]
  (let [input (util/load-input (str "resources/day16/" filename))
        bin-packet (part1/hex-string-to-bin (first input))]
    (parse-packet bin-packet)))