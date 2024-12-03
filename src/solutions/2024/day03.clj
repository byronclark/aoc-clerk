^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day03
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [util :as u]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "03" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; Load our input. No parsing to be done at read, it's a giant string.
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(def input (slurp (io/resource "inputs/2024/day03.txt")))

;; Here's what the beginning looks like:
{:nextjournal.clerk/visibility {:code :show :result :show}}
(subs input 0 60)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; Is it time for regex? I think it is.
(defn mult-instructions
  [memory]
  (->> memory
       (re-seq #"mul\((\d+),(\d+)\)")
       (map rest)
       (map #(map parse-long %))))

(defn part-1
  [input]
  (->> input
       mult-instructions
       (map #(* (first %) (second %)))
       (apply +)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
;;
;; The simple approach is to find all multiply instructions between a `do()` and a
;; `don't()` but that doesn't work because they aren't guaranteed to be in pairs. Instead
;; we'll have to loop through ourselves and track the enabled state.
;;
;; Since we already know how to find the multiply instructions from part 1, we can limit
;; our effort in part 2 to finding the enabled sections and then reusing part 1 to find
;; the multiply instructions in those sections.
(defn enabled-sections
  [memory]
  (loop [remaining memory
         found []
         enabled? true
         current []]
    (cond
      (str/blank? remaining)
      (if enabled?
        (conj found (str/join "" current))
        found)

      (str/starts-with? remaining "do()")
      (recur (subs remaining 4)
             found
             true
             (if enabled? current []))

      (str/starts-with? remaining "don't()")
      (recur (subs remaining 6)
             (if enabled?
               (conj found (str/join "" current))
               found)
             false
             [])

      :else
      (recur (subs remaining 1)
             found
             enabled?
             (if enabled?
               (conj current (subs remaining 0 1))
               current)))))

(defn part-2
  [input]
  (->> input
       enabled-sections
       (mapcat mult-instructions)
       (map #(* (first %) (second %)))
       (apply +)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 input)
