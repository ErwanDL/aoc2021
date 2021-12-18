(ns aoc2021.day16.part1
  (:require [aoc2021.util :as util]))

(def hex-to-bin {\0 "0000", \1 "0001", \2 "0010", \3 "0011"
                 \4 "0100", \5 "0101", \6 "0110", \7 "0111"
                 \8 "1000", \9 "1001", \A "1010", \B "1011"
                 \C "1100", \D "1101", \E "1110", \F "1111"})

(defn hex-string-to-bin [hex-string]
  (->> hex-string
       (reduce #(concat %1 (get hex-to-bin %2)) '())
       (map #(Character/digit % 10))))

(defn parse-packet-header [data]
  (let [[header tail] (split-at 6 data)
        [version type-id] (mapv util/binary-to-decimal (split-at 3 header))]
    [version type-id tail]))

(defn parse-subpackets-length [data]
  (let [[fifteen tail] (split-at 15 data)]
    [(util/binary-to-decimal fifteen) tail]))

(defn parse-subpacket-count [data]
  (let [[eleven tail] (split-at 11 data)]
    [(util/binary-to-decimal eleven) tail]))

(defn parse-literal-packet [data]
  (loop [[[first-bit & four-bits] tail] (split-at 5 data)
         number []
         n-bits-seen 11]
    (let [updated-number (concat number four-bits)]
      (if (zero? first-bit)
        [{:value (util/binary-to-decimal updated-number)
          :length n-bits-seen} tail]
        (recur (split-at 5 tail) updated-number (+ 5 n-bits-seen))))))

(defn parse-zero-ltid [data version parse-packet-fn]
  (let [[subpackets-length tail] (parse-subpackets-length data)]
    (loop [remaining subpackets-length
           accumulated-length 22 ; 7 + 15       
           version-sum version
           tail tail]
      (if (zero? remaining)
        [{:version-sum version-sum
          :length accumulated-length} tail] ; 7 + 15
        (let [[packet-info tail] (parse-packet-fn tail)]
          (recur (- remaining (get packet-info :length))
                 (+ accumulated-length (get packet-info :length))
                 (+ version-sum (get packet-info :version-sum))
                 tail))))))

(defn parse-one-ltid [data version parse-packet-fn]
  (let [[subpacket-count tail] (parse-subpacket-count data)]
    (loop [remaining subpacket-count
           accumulated-length 18 ; 7 + 11
           version-sum version
           tail tail]
      (if (zero? remaining)
        [{:version-sum version-sum
          :length accumulated-length} tail]
        (let [[packet-info tail] (parse-packet-fn tail)]
          (recur (dec remaining)
                 (+ accumulated-length (get packet-info :length))
                 (+ version-sum (get packet-info :version-sum))
                 tail))))))

(defn parse-packet [data]
  (let [[version type-id tail] (parse-packet-header data)]
    (if (= 4 type-id)
      (let [[packet-info tail] (parse-literal-packet tail)]
        [(assoc packet-info :version-sum version) tail])
      (let [[length-type-id & tail] tail]
        (if (zero? length-type-id)
          (parse-zero-ltid tail version parse-packet)
          (parse-one-ltid tail version parse-packet))))))

(defn run [filename]
  (let [input (util/load-input (str "resources/day16/" filename))
        bin-packet (hex-string-to-bin (first input))]
    (parse-packet bin-packet)))