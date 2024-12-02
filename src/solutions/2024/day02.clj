^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day02
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [util :as u]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "02" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :hide}}

;; # Solution
;;
;; Load and parse our input. Each report is a list of integers.
(defn parse-report [line]
  (map parse-long (str/split line #" ")))

(def input (->> (io/resource "inputs/2024/day02.txt")
                slurp
                str/split-lines
                (map parse-report)))

;; Sample of the parsed input
{:nextjournal.clerk/visibility {:code :show :result :show}}
(take 2 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; The simplest path is taking each pair of numbers and subtracting the second from the
;; first. Once we have those values we can check that they all have the same sign and have
;; an absolute value between 1 and 3.
(defn safe?
  [report]
  (let [gaps (->> report
                  (partition 2 1)
                  (map #(apply - %)))]
    (and (or (every? pos? gaps)
             (every? neg? gaps))
         (every? #(<= 1 (abs %) 3) gaps))))

(defn part-1
  [reports]
  (->> reports
       (filter safe?)
       count))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
;;
;; Given the size of the data set we can brute force this by checking unsafe reports with
;; each level removed.
(defn dampened [report]
  (for [n (range (count report))]
    (concat (take n report) (drop (inc n) report))))

(defn part-2
  [reports]
  (->> reports
       (filter #(or (safe? %)
                    (some safe? (dampened %))))
       count))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 input)
