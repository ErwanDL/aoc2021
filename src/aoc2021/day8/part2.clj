(ns aoc2021.day8.part2
  (:require
   [aoc2021.util :as util]
   [aoc2021.day8.part1 :as part1]
   clojure.set))

(def segments-to-digits {(set "abcefg") \0
                         (set "cf") \1
                         (set "acdeg") \2
                         (set "acdfg") \3
                         (set "bcdf") \4
                         (set "abdfg") \5
                         (set "abdefg") \6
                         (set "acf") \7
                         (set "abcdefg") \8
                         (set "abcdfg") \9})

(defn count-nb-of-segments [patterns]
  (reduce #(update %1 (count %2) conj %2) {} patterns))

(defn find-a [counted-patterns]
  (first (clojure.set/difference (set (first (get counted-patterns 3)))
                                 (set (first (get counted-patterns 2))))))
(defn find-b-or-d [counted-patterns]
  (clojure.set/difference (set (first (get counted-patterns 4)))
                          (set (first (get counted-patterns 2)))))
(defn find-b-or-e [counted-patterns]
  (let [five-segment-patterns (get counted-patterns 5)]
    (->> five-segment-patterns
         (reduce concat)
         (reduce #(update %1 %2 (fnil inc 0)) {})
         (filter #(= 1 (second %)))
         (map first)
         (set))))

(defn find-c-or-e-or-d [counted-patterns]
  (let [six-segment-patterns (get counted-patterns 6)]
    (->> six-segment-patterns
         (reduce concat)
         (reduce #(update %1 %2 (fnil inc 0)) {})
         (filter #(= 2 (second %)))
         (map first)
         (set))))

(defn find-e-or-g [counted-patterns]
  (let [four (first (get counted-patterns 4))
        seven (first (get counted-patterns 3))]
    (clojure.set/difference (set "abcdefg")
                            (set (concat four seven)))))

(defn find-mappings [patterns]
  (let [counted-patterns (count-nb-of-segments patterns)
        a (find-a counted-patterns)
        c-or-f (set (first (get counted-patterns 2)))
        b-or-d (find-b-or-d counted-patterns)
        b-or-e (find-b-or-e counted-patterns)
        e-or-g (find-e-or-g counted-patterns)
        c-or-e-or-d (find-c-or-e-or-d counted-patterns)
        b (first (clojure.set/intersection b-or-d b-or-e))
        d (first (disj b-or-d b))
        e (first (disj b-or-e b))
        g (first (disj e-or-g e))
        c (first (clojure.set/intersection c-or-f c-or-e-or-d))
        f (first (disj c-or-f c))]
    {a \a, b \b, c \c, d \d, e \e, f \f, g \g}))

(defn decode-digit [output mappings]
  (get segments-to-digits (set (map #(get mappings %) output))))

(defn decode-number [line]
  (let [[patterns outputs] line
        mappings (find-mappings patterns)]
    (->> outputs
         (map #(decode-digit % mappings))
         (apply str)
         (Integer/parseInt))))

(defn run [filename]
  (let [input (util/load-input (str "resources/day8/" filename))
        lines (map part1/parse-input input)]
    (reduce + (map decode-number lines))))