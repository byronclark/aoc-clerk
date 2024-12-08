^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day06
  {:nextjournal.clerk/toc true}
  (:require
   [better-cond.core :as b]
   [clojure.java.io :as io]
   [nextjournal.clerk :as clerk]
   [solutions.2024.helpers.grid :as grid]
   [util :as u]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
^::clerk/no-cache
(clerk/html (u/load-problem "06" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :hide}}

;; # Solution
;;
;; Load and parse our input
;;
;; Make sure we give the items some better names:
(defn ->entity
  [c]
  (case c
    \. :floor
    \# :obstruction
    \^ :guard))

{:nextjournal.clerk/visibility {:code :show :result :show}}
(def input (-> (io/resource "inputs/2024/day06.txt")
               slurp
               (grid/character-lines->grid :convert ->entity)))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; Both parts use the same logic for patrolling, although Part 1 doesn't require the check
;; for a loop. But we'll include it here anyway.
(defn patrol
  "Patrol the lab from start until the guard leaves to map or a loop is detected.

  Returns {:path [loc ...] :loop? false}"
  [lab start facing]
  (let [next-space (fn [[x y] facing]
                     (case facing
                       :up [x (dec y)]
                       :right [(inc x) y]
                       :down [x (inc y)]
                       :left [(dec x) y]))
        next-facing {:up :right
                     :right :down
                     :down :left
                     :left :up}]
    (loop [current [start facing]
           path []
           visited #{}]
      (b/cond
        :let [[position facing] current]
        (not (grid/valid-location? lab position))
        {:path path :loop? false}

        (contains? visited current)
        {:path path :loop? true}

        :let [position' (next-space position facing)]
        (= :obstruction (grid/at lab position'))
        (recur [position (next-facing facing)]
               path
               (conj visited current))

        :else
        (recur [position' facing]
               (conj path position)
               (conj visited current))))))

(defn start-position
  [lab]
  (->> (grid/all-locations lab)
       (filter #(= :guard (grid/at lab %)))
       first))

(defn part-1
  [lab]
  (let [{:keys [path]} (patrol lab (start-position lab) :up)]
    (->> path
         set
         count)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
;;
;; There's probably a smarter way to do this, but for a first attempt I think we want to
;; try all the positions that the guard traveled in part 1, place an obstacle there, and
;; see if the guard gets stuck in a loop.
;;
;; So, how do we detect a loop?
;; - Correct: Have we visited this spot facing the same direction?
;; - Cheating: Is the path longer than ~1000~ 10000 steps?
;;
;; I was surprised to find both how long the paths were and how much slower the correct
;; solution was.
(defn part-2
  [lab]
  (let [start (start-position lab)
        {initial-path :path} (patrol lab start :up)]
    (->> initial-path
         (drop 1)                       ;Can't start at the guard position
         set                            ;No duplicates
         (pmap (fn [pos]
                 (-> (grid/set-at lab pos :obstruction)
                     (patrol start :up)
                     :loop?)))
         (keep identity)
         count)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 input)
