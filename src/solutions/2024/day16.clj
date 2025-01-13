^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day16
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [util :as u]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]
   [solutions.2024.helpers.grid :as grid]
   [better-cond.core :as bc]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "16" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; Load and parse our input
(def tiles {\# :wall
            \. :floor
            \S :start
            \E :end})

(def input (-> (io/resource "inputs/2024/day16.txt")
               slurp
               (grid/character-lines->grid :convert tiles)))

;; And a smaller test
(def test-input (grid/character-lines->grid "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############" :convert tiles))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; It's time for pathfinding! Let's use A*, but I'm still not sure what our heuristic will be.
;;
;; To make this work, we need to build the right graph. Instead of locations connected to each other with equal cost, need to account for direction. For example:
;; - `[[0 0] :right] -> [[1 0] :any]` = Cost 1
;; - `[[0 0] :right] -> [[0 0] :down]` = Cost 1000
;; - `[[0 0] :right] -> [[0 0] :up]` = Cost 1000
;;
(defn- adjacent
  [[x y] direction]
  (case direction
    :up [x (dec y)]
    :right [(inc x) y]
    :down [x (inc y)]
    :left [(dec x) y]))

;; Build out all the valid connections from a single location along with our cost.
(defn- connections
  [maze location]
  (let [turns {:up [:left :right]
               :right [:up :down]
               :down [:right :left]
               :left [:down :up]}]
    (->> [:up :right :down :left]
         (mapcat (fn [direction]
                   (conj (->> turns
                              direction
                              (map (fn [turn]
                                     [{:point location :direction direction}
                                      {:point location :direction turn :cost 1000}])))
                         [{:point location :direction direction}
                          {:point (adjacent location direction) :direction direction :cost 1}])))
         (filter (fn [[_ {:keys [point]}]]
                   (when-let [tile (grid/at maze point)]
                     (not= :wall tile)))))))

(defn ->graph
  [maze]
  (reduce (fn [graph location]
            (let [tile (grid/at maze location)]
              (if (= :wall tile)
                graph
                (reduce (fn [graph [source destination]]
                          (update graph source conj destination))
                        graph
                        (connections maze location)))))
          {}
          (grid/all-locations maze)))

;; And now we get to A* which we can build somewhat generically for when we need it again
;; later. But we don't have a heuristic so it's really Dijkstra's algorithm with a fancy
;; coat of paint.
(defn a*
  [graph start-location {:keys [target-fn cost-fn done?-fn]}]
  (let [MAX-VALUE (Long/MAX_VALUE)]
    (loop [open-set #{start-location}
           g-scores {start-location 0}]
      (bc/cond
        (empty? open-set) ;no path available
        nil

        :let [current (apply min-key #(get g-scores % MAX-VALUE) open-set)
              current-g-score (get g-scores current)]
        (done?-fn current)
        current-g-score

        :let [[open-set' g-scores']
              (reduce (fn [[open-set g-scores] connection]
                        (let [target (target-fn connection)
                              test-g-score (+ current-g-score (cost-fn connection))]
                          (if (< test-g-score (get g-scores target MAX-VALUE))
                            [(conj open-set target) (assoc g-scores target test-g-score)]
                            [open-set g-scores])))
                      [(disj open-set current) g-scores]
                      (get graph current))]
        :else
        (recur open-set' g-scores')))))

;; Most of the callback/helper functions here are so that I don't have to worry about
;; building different shapes of the data to figure out cost of each move and target moves.
(defn part-1
  [maze]
  (let [graph (->graph maze)
        start-location {:point (first (grid/find-value maze :start))
                        :direction :right}
        target-location (first (grid/find-value maze :end))]
    (a* graph start-location {:target-fn #(select-keys % [:direction :point])
                              :cost-fn :cost
                              :done?-fn #(= (:point %) target-location)})))

;; Which gives our answer for the test input
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 test-input)

;; And the full input
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
(defn part-2
  [input]
  (println "Part 2"))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 input)
