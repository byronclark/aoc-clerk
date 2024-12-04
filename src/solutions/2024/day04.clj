^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day04
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [nextjournal.clerk :as clerk]
   [solutions.2024.helpers.grid :as grid]
   [util :as u]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
^::clerk/no-cache
(clerk/html (u/load-problem "04" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First grid based problem of the year. Time to bring back grid utilities from years
;; past.
;;
;; Load the input into a grid of characters.
(def input (->> (io/resource "inputs/2024/day04.txt")
                slurp
                grid/character-lines->grid))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; Let's keep it simple this time. We'll walk through the grid and any time we hit an X,
;; fetch the three remaining characters in each possible direction and check if we have
;; XMAS.
(defn words-from
  [grid length location]
  (when (= \X (grid/at grid location))
    (let [[x y] location]
      [(map #(grid/at grid [x (- y %)]) (range length)) ;up
       (map #(grid/at grid [(+ x %) y]) (range length)) ;right
       (map #(grid/at grid [x (+ y %)]) (range length)) ;down
       (map #(grid/at grid [(- x %) y]) (range length)) ;left
       (map #(grid/at grid [(- x %) (- y %)]) (range length)) ;up-left
       (map #(grid/at grid [(+ x %) (- y %)]) (range length)) ;up-right
       (map #(grid/at grid [(+ x %) (+ y %)]) (range length)) ;down-right
       (map #(grid/at grid [(- x %) (+ y %)]) (range length)) ;down-left
       ])))

(defn part-1
  [word-search]
  (->> (grid/all-locations word-search)
       (mapcat (partial words-from word-search 4))
       (filter #(= [\X \M \A \S] %))
       count))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
;;
;; We're grabbing different sets of characters from each A for the words and checking that
;; both of them are MAS or SAM.
;;
;; In this case, it's probably easiest to list the possible locations for the cross
;; instead of finding a clever way to generate them.
;;
;; In this case *only* the diagonals count as a cross. Apparently horizontal and vertical
;; together make a plus sign instead of a cross.
(defn possible-xs-from
  [grid location]
  (when (= \A (grid/at grid location))
    (let [[x y] location]
      [(map (partial grid/at grid) [[(dec x) (dec y)] [x y] [(inc x) (inc y)]])
       (map (partial grid/at grid) [[(dec x) (inc y)] [x y] [(inc x) (dec y)]])])))

(defn part-2
  [word-search]
  (->> (grid/all-locations word-search)
       (keep (partial possible-xs-from word-search))
       (filter #(every? #{[\S \A \M] [\M \A \S]} %))
       count))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :show :result :show}}
(part-2 input)
