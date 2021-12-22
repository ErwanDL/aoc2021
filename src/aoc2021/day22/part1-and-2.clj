(ns aoc2021.day22.part1-and-2
  (:require [aoc2021.util :as util]))

(def instr-regex #"(on|off) x=(-?[0-9]+)..(-?[0-9]+),y=(-?[0-9]+)..(-?[0-9]+),z=(-?[0-9]+)..(-?[0-9]+)")

(defn parse-line [line]
  (let [match (re-matches instr-regex line)
        on? (= "on" (second match))]
    [on? [(Integer/parseInt (nth match 2)) (Integer/parseInt (nth match 3))]
     [(Integer/parseInt (nth match 4)) (Integer/parseInt (nth match 5))]
     [(Integer/parseInt (nth match 6)) (Integer/parseInt (nth match 7))]]))

(defn intervals-intersect? [[x y] [x' y']]
  (and (<= x' y) (<= x y')))

(defn cubes-intersect? [cube-a cube-b]
  (let [[x y z] cube-a
        [x' y' z'] cube-b]
    (and (intervals-intersect? x x')
         (intervals-intersect? y y')
         (intervals-intersect? z z'))))

(defn create-top-cube [[x y z] [_x' _y' z'] [cubes _built]]
  (if (< (second z') (second z))
    [(conj cubes [x y [(inc (second z')) (second z)]]) #{:top}]
    [cubes _built]))

(defn create-bottom-cube [[x y z] [_x' _y' z'] [cubes built]]
  (if (> (first z') (first z))
    [(conj cubes [x y [(first z) (dec (first z'))]]) (conj built :bottom)]
    [cubes built]))

(defn z-coord [z z' built]
  [(if (contains? built :bottom) (first z') (first z))
   (if (contains? built :top) (second z') (second z))])

(defn create-right-cube [[x y z] [_x' y' z'] [cubes built]]
  (if (< (second y') (second y))
    [(conj cubes [x [(inc (second y')) (second y)] (z-coord z z' built)])
     (conj built :right)]
    [cubes built]))

(defn create-left-cube [[x y z] [_x' y' z'] [cubes built]]
  (if (> (first y') (first y))
    [(conj cubes [x [(first y) (dec (first y'))] (z-coord z z' built)])
     (conj built :left)]
    [cubes built]))

(defn y-coord [y y' built]
  [(if (contains? built :left) (first y') (first y))
   (if (contains? built :right) (second y') (second y))])

(defn create-front-cube [[x y z] [x' y' z'] [cubes built]]
  (if (< (second x') (second x))
    [(conj cubes [[(inc (second x')) (second x)] (y-coord y y' built) (z-coord z z' built)])
     (conj built :front)]
    [cubes built]))

(defn create-back-cube [[x y z] [x' y' z'] [cubes built]]
  (if (> (first x') (first x))
    [(conj cubes [[(first x) (dec (first x'))] (y-coord y y' built) (z-coord z z' built)])
     (conj built :back)]
    [cubes built]))

(defn chomp-cube [to-chomp chomper cubes]
  (first (->> [cubes #{}]
              (create-top-cube to-chomp chomper)
              (create-bottom-cube to-chomp chomper)
              (create-right-cube to-chomp chomper)
              (create-left-cube to-chomp chomper)
              (create-front-cube to-chomp chomper)
              (create-back-cube to-chomp chomper))))

(defn apply-instr [cubes instr]
  (let [[on? & cube] instr
        affected-cubes? (group-by #(cubes-intersect? % cube) cubes)
        after-chomps (reduce #(chomp-cube %2 cube %1)
                             (get affected-cubes? false) (get affected-cubes? true))]
    (if on? (conj after-chomps cube) after-chomps)))

(defn volume [cube]
  (reduce #(* %1 (- (inc (second %2)) (first %2))) 1 cube))

(defn count-cubies [cubes]
  (reduce #(+ %1 (volume %2)) 0 cubes))

(defn run [filename]
  (let [input (util/load-input (str "resources/day22/" filename))
        instrs (map parse-line input)
        final-cubes (reduce apply-instr [] instrs)]
    (count-cubies final-cubes)))