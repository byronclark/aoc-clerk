^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day25
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [clojure.math.combinatorics :as combo]
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]
   [util :as u]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "25" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :hide}}


;; # Solution
;;
;; Load and parse our input
(defn- pin-height
  [schematic-lines column]
  (->> schematic-lines
       (filter #(= \# (get % column)))
       count
       dec))

(defn- parse-input
  [raw-input]
  (let [lock? #(= \# (first %))
        schematics (str/split raw-input #"\n\n")]
    (->> schematics
         (map (fn [schematic]
                (let [schematic-lines (str/split-lines schematic)]
                  [(if (lock? schematic) :lock :key)
                   (mapv #(pin-height schematic-lines %)
                         (range (count (first schematic-lines))))])))
         (group-by first)
         (map (fn [[schematic-type schematics]]
                [schematic-type (map second schematics)]))
         (into {}))))

{:nextjournal.clerk/visibility {:code :show :result :show}}
(def input (-> (io/resource "inputs/2024/day25.txt")
               slurp
               parse-input))

;; And the smaller test input
(def test-input (parse-input "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####"))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; Famous last words: this is straightforward. Add the pin-heights in each
;; column, if any column is > 5 then the key doesn't fit.
(defn- overlaps?
  [lock key]
  (->> (map + lock key)
       (some #(> % 5))))

(defn part-1
  [schematics]
  (->> (combo/cartesian-product (:lock schematics) (:key schematics))
       (remove #(apply overlaps? %))
       count))

;; Which gives our answer with the test input
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 test-input)

;; And the full input
(part-1 input)
