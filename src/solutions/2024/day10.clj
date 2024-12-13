^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day10
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [nextjournal.clerk :as clerk]
   [solutions.2024.helpers.grid :as grid]
   [util :as u]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
^::clerk/no-cache
(clerk/html (u/load-problem "10" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; Load and parse our input
(def input (-> (io/resource "inputs/2024/day10.txt")
               slurp
               (grid/character-lines->grid :convert grid/char->digit)))

;; And a smaller test input
(def test-input (grid/character-lines->grid "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732" :convert grid/char->digit))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; To score the trailheads, we probably need to start by finding them.
(defn trailheads
  [topomap]
  (filter #(zero? (grid/at topomap %))
          (grid/all-locations topomap)))

;; We need the same recursive walk function for both parts. The only difference is what
;; they return when reaching a peak.
(defn score-trailhead
  [mode topomap pos]
  (let [inner
        (fn inner [visited pos]
          (let [height (grid/at topomap pos)
                visited' (conj visited pos)]
            (if (= height 9)
              (case mode
                :peaks [pos]
                :trails [visited'])
              (let [[x y] pos]
                (->> [[(dec x) y] [x (dec y)] [(inc x) y] [x (inc y)]]
                     (filter #(and (not (contains? visited' %))
                                   (= (inc height) (grid/at topomap %))))
                     (mapcat (partial inner visited'))
                     set)))))]
    (inner #{} pos)))

;; For Part 1 we count the peaks we can reach from the trailhead
(defn reachable-peaks
  [topomap pos]
  (score-trailhead :peaks topomap pos))

(defn part-1
  [topomap]
  (->> topomap
       trailheads
       (map (partial reachable-peaks topomap))
       (map count)
       (apply +)))

;; Which gives our test answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 test-input)

;; And our final answer
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
;;
;; Part 2 turns out to be the same walking function as Part 1 but we return each path
;; instead of each peak.
(defn trails-to-peaks
  [topomap pos]
  (score-trailhead :trails topomap pos))

(defn part-2
  [topomap]
  (->> topomap
       trailheads
       (map (partial trails-to-peaks topomap))
       (map count)
       (apply +)))

{:nextjournal.clerk/visibility {:code :hide :result :show}}
;; Which gives our test answer
(part-2 test-input)

;; And our final answer
(part-2 input)
