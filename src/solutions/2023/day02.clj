^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2023.day02
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [util :as u]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "02" "2023"))
{:nextjournal.clerk/visibility {:code :show :result :hide}}

;; # Solution
;;
;; Load and parse our input
(defn parse-draw
  [draw]
  (->> (str/split draw #", ")
       (map #(let [[n color] (str/split % #" " 2)]
               [(keyword color) (parse-long n)]))
       (into {})))

(defn parse-game
  [line]
  (let [[_ draws] (str/split line #": " 2)]
    (->> (str/split draws #"; ")
         (map parse-draw))))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
(def input (->> (io/resource "inputs/2023/day02.txt")
                slurp
                str/split-lines
                (map parse-game)))
{:nextjournal.clerk/visibility {:code :show :result :show}}
(first input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
(defn possible?
  [game]
  (every? (fn [{:keys [red green blue] :or {red 0 green 0 blue 0}}]
            (and (<= red 12)
                 (<= green 13)
                 (<= blue 14)))
          game))

(defn part-1
  [input]
  (->> input
       (map-indexed vector)
       (keep (fn [[game-id game]] (when (possible? game) (inc game-id))))
       (apply +)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
(defn power
  [game]
  (let [{:keys [red green blue]}
        (reduce (fn [maximums {:keys [red green blue] :or {red 0 green 0 blue 0}}]
                  (-> maximums
                      (update :red max red)
                      (update :green max green)
                      (update :blue max blue)))
                {:red 0 :green 0 :blue 0} game)]
    (* red green blue)))

(defn part-2
  [input]
  (->> input
       (map power)
       (apply +)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 input)
