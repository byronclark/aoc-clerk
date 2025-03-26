^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day20
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [nextjournal.clerk :as clerk]
   [solutions.2024.helpers.grid :as grid]
   [solutions.2024.helpers.pathfinding :refer [a* a*-with-path]]
   [util :as u]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "20" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; Load and parse our input
(defn ->entity [c]
  (case c
    \. :floor
    \# :wall
    \S :start
    \E :end))

(def input (-> (io/resource "inputs/2024/day20.txt")
               slurp
               (grid/character-lines->grid :convert ->entity)))

;; And a smaller test input
(def test-input (grid/character-lines->grid "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############" :convert ->entity))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; This flies in the face of any ideas about efficiency, but I want to try locating all possible cheats and then seeing how much time each of them save.
;;
;; A cheat is start position and an end position (both on the track) separated by a wall space. Each cheat has a cost of two picoseconds.
(defn- possible-cheats-from
  "Returns pairs of locations like [wall landing] that could be cheats if wall is a wall and landing is not."
  [[x y]]
  [[[(inc x) y] [(+ x 2) y]]              ;right
   [[x (inc y)] [x (+ y 2)]]              ;down
   [[(dec x) y] [(- x 2) y]]              ;left
   [[x (dec y)] [x (- y 2)]]              ;up
   ])

(defn space-class
  [racetrack location]
  (case (grid/at racetrack location)
    :wall :wall
    (:start :end :floor) :floor
    nil))

(defn find-cheats-at
  [racetrack location]
  (when (not= :wall (grid/at racetrack location))
    (->> (possible-cheats-from location)
         (filter (fn [[wall landing]]
                   (and (= :wall (space-class racetrack wall))
                        (= :floor (space-class racetrack landing)))))
         (map (fn [[_ landing]]
                {:destination landing :cost 2})))))

;; For comparison and path finding we need to collect all the routes in the map.
(defn ->graph
  [racetrack]
  (let [floor-from (fn [from]
                     (filter #(= :floor (space-class racetrack %))
                             (grid/adjacent-locations racetrack from)))]
    (->> (grid/all-locations racetrack)
         (remove #(= :wall %))
         (map (fn [location]
                [location (->> location
                               floor-from
                               (map (fn [dest-location] {:destination dest-location :cost 1})))]))
         (into {}))))

;; My first attempt was to apply each cheat to the map and see how long the
;; resulting shortest path was. The code was simple and worked for the small
;; test case. But for the actual large case it took way too long to run through
;; the 10000+ cheats.

;; So we'll make an assumption about the test and hope that it holds:
;; - The only cheats that matter start and end on the shortest path
;;
;; If this assumption holds, we can determine savings for each cheat by how many
;; spaces there are between the start and end location on the path.
(defn part-1
  [racetrack min-savings]
  (let [start-location (first (grid/find-value racetrack :start))
        end-location (first (grid/find-value racetrack :end))
        base-track-graph (->graph racetrack)
        possible-cheats (->> (grid/find-value racetrack :floor)
                             (concat [start-location])
                             (map #(vec [% (find-cheats-at racetrack %)]))
                             (into {}))
        shortest-path (first (a*-with-path start-location
                                           {:connections-fn #(get base-track-graph %)
                                            :target-fn :destination
                                            :cost-fn :cost
                                            :done?-fn #(= % end-location)}))
        path-location-index (->> shortest-path
                                 (map-indexed #(vector %2 %1))
                                 (into {}))]
    (->> (for [[cheat-start cheats] possible-cheats
               :let [start-index (get path-location-index cheat-start)]
               cheat cheats
               :let [end-index (get path-location-index (:destination cheat))]]
           ;; The amount saved is the number of indexes between the two locations
           (- (dec end-index) (inc start-index)))
         (filter #(>= % min-savings))
         count)))

;; Which gives our answer for the test input using 2 as the minimum cheat
;; savings because there aren't any that save 100.
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 test-input 2)

;; And the full input
(part-1 input 100)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
(defn part-2
  [input]
  (println "Part 2"))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 input)
