^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day01
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [util :as u]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "01" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :hide}}

;; # Solution
;;
;; Load and parse our input
(def input (->> (io/resource "inputs/2024/day01.txt")
                slurp
                str/split-lines
                (reduce (fn [[list-a list-b] line]
                          (let [locations (map parse-long (str/split line #"\s+" 2))]
                            [(conj list-a (first locations))
                             (conj list-b (second locations))]))
                        [[] []])))

{:nextjournal.clerk/visibility {:code :show :result :show}}
;; Trimmed input after parsing
(mapv #(vec (take 5 %)) input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; To find the difference, we sort the lists and then iterate finding the distance between
;; each pair of elements.
(defn part-1 [[list-a list-b]]
  (apply + (map #(abs (- %1 %2)) (sort list-a) (sort list-b))))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
;;
;; We'll use `frequencies` to find how often each location occurs in the right side and
;; use it as a cache while checking each item in the left side.
(defn part-2 [[left right]]
  (let [occurs (frequencies right)]
    (->> left
         (keep #(when-let [c (get occurs %)]
                  (* % c)))
         (apply +))))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 input)
