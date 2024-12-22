^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day12
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [nextjournal.clerk :as clerk]
   [solutions.2024.helpers.grid :as grid]
   [util :as u]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "12" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; Surprise, it's another grid!
(def input (-> (io/resource "inputs/2024/day12.txt")
                slurp
                grid/character-lines->grid))

;; And the smaller input from the description.
(def test-input (-> "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"
                    grid/character-lines->grid))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; It may not be efficient, but I'm going to divide the garden into regions in one step and then calculate the perimeter of each region.
;;
;; Given a location we can recurse in all directions to find the plots in the region.
(defn- explore-region
  ([garden start]
   (explore-region garden start (grid/at garden start) #{}))
  ([garden position plot-type found]
   (if-not (= plot-type (grid/at garden position))
     found
     (->> position
          (grid/adjacent-locations garden)
          (reduce (fn [found' next-pos]
                    (if (contains? found' next-pos)
                      found'
                      (explore-region garden next-pos plot-type found')))
                  (conj found position))))))

;; And now we can walk each space and build a collection of regions
(defn- find-regions [garden]
  (first (reduce (fn [[regions visited] position]
                   (if (contains? visited position)
                     [regions visited]
                     (let [region (explore-region garden position)]
                       [(conj regions region)
                        (set/union visited region)])))
                 [[] #{}]
                 (grid/all-locations garden))))

;; To calculate the perimeter of a region, we see how many adjacent squares are in the
;; same region and subtract from the maximum possible perimeter
(defn- perimeter [garden region]
  (->> region
       (map (fn [position]
              (->> (grid/adjacent-locations garden position)
                   (filter #(contains? region %))
                   count)))
       (apply +)
       (- (* 4 (count region)))))

(defn part-1
  [garden]
  (->> garden
       find-regions
       (map (fn [region] (* (count region) (perimeter garden region))))
       (apply +)))

;; Which gives our answer for the test input
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 test-input)

;; And for the full input
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
;;
;; Counting the sides seems way more challenging. My first thought was counting only the fence edges on locations with >= 2 exposed edges, but that double counts connected the corners on a straight line. We could count each column or row with a fence, but that breaks when there's a gap.
;;
;; I think we're going to have find all the fences and then see what's connected. Finding the fences is a little more involved because we have to track which side they're on to match contiguous regions.
(defn- plot-fences
  "Returns all the fences for a given plot position in region.

  Fences have the form [[side line] position].
  - for vertical fences (:left or :right): line is the column (x) and position is the row (y)
  - for horizontal fences (:up or :down): line is the row (y) and position is the column (x)

  The goal of the odd output is to easily collect all the segments on a particular fence line."
  [region plot]
  (let [[x y] plot]
    (->> [(when-not (contains? region [(dec x) y]) [[:left x] y])
          (when-not (contains? region [x (dec y)]) [[:up y] x])
          (when-not (contains? region [(inc x) y]) [[:right x] y])
          (when-not (contains? region [x (inc y)]) [[:down y] x])]
         (keep identity))))

;; Once we've found all the fences in a way that's easy to turn into fence-lines we can sort and then count discontinuities.
(defn- count-segments [line]
  (->> line
       sort
       (partition 2 1)
       (filter #(< 1 (- (second %) (first %))))
       count
       inc))                            ;There's at least one fence


(defn- sides [region]
  (->> region
       (mapcat #(plot-fences region %))
       (group-by first)
       (map second)                     ;Remove the group-by key
       (map #(map second %))            ;Keep only the positions
       (map count-segments)
       (apply +)))

(defn part-2
  [garden]
  (->> garden
       find-regions
       (map (fn [region] (* (count region)
                            (sides region))))
       (apply +)))

;; Which gives our answer for the test input
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 test-input)

;; And for the full input
(part-2 input)
