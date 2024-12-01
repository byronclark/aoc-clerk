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
                          (let [[loc-a loc-b] (str/split line #"\s+" 2)]
                            [(conj list-a (parse-long loc-a))
                             (conj list-b (parse-long loc-b))]))
                        [[] []])))
{:nextjournal.clerk/visibility {:code :show :result :show}}
(mapv #(vec (take 5 %)) input)


{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
(defn part-1 [[list-a list-b]]
  (reduce (fn [acc [a b]]
                 (+ acc (abs (- a b))))
          0
          (zipmap (sort list-a) (sort list-b))))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
(defn part-2 [[list-a list-b]]
  (let [occurs (frequencies list-b)]
    (->> list-a
         (map #(* % (get occurs % 0)))
         (reduce + 0))))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 input)
