^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day08
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [clojure.math.combinatorics :as combo]
   [nextjournal.clerk :as clerk]
   [solutions.2024.helpers.grid :as grid]
   [util :as u]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "08" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; Load and parse our input. Yet another grid of characters.
(def input (->> (io/resource "inputs/2024/day08.txt")
                slurp
                grid/character-lines->grid))

;; And the smaller input for testing
(def test-input (grid/character-lines->grid "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; We'll start by finding all the antennas for each frequency.
(defn antennas
  [roof]
  (reduce (fn [found pos]
            (let [roof-square (grid/at roof pos)]
              (if (= \. roof-square)
                found
                (update found roof-square conj pos))))
          {}
          (grid/all-locations roof)))

;; Then for given frequency we can pair all the antennas to calculate where the antinodes
;; would be.
(defn antinodes
  [antennas]
  (->> (combo/combinations antennas 2)
       (mapcat (fn [[[x1 y1] [x2 y2]]]
                 (let [xdiff (- x2 x1)
                       ydiff (- y2 y1)]
                   [[(+ x2 xdiff) (+ y2 ydiff)]
                    [(- x1 xdiff) (- y1 ydiff)]])))))
(defn part-1
  [roof]
  (->> (antennas roof)
       (mapcat (fn [[_frequncy locations]]
                 (antinodes locations)))
       set
       (filter (partial grid/valid-location? roof))
       count))

;; Which gives our test input answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 test-input)

;; And our full input answer
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
;;
;; The big change for this part is that we keep calculating antinodes as long as we're
;; still within the grid.
;;
;; I'm a little worried about this in Clojure because map from a range is chunked not
;; lazy. But apparently it's fast enough.
(defn antinodes-repeating
  [roof antennas]
  (->> (combo/combinations antennas 2)
       (mapcat (fn [[[x1 y1] [x2 y2]]]
                 (let [xdiff (- x2 x1)
                       ydiff (- y2 y1)]
                   (concat
                    ;; down and to the right
                    (->> (range)
                         (map inc)
                         (map #(vector (+ x2 (* % xdiff)) (+ y2 (* % ydiff))))
                         (take-while (partial grid/valid-location? roof)))
                    ;; up and to the left
                    (->> (range)
                         (map inc)
                         (map #(vector (- x1 (* % xdiff)) (- y1 (* % ydiff))))
                         (take-while (partial grid/valid-location? roof)))))))
       (concat antennas)))

(defn part-2
  [roof]
  (->> (antennas roof)
       (mapcat (fn [[_frequency locations]]
                 (antinodes-repeating roof locations)))
       set
       count))

;; Which gives our test input answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 test-input)

;; And our full input answer
(part-2 input)
