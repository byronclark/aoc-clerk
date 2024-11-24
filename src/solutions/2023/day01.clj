^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2023.day01
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [util :as u]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]
   [better-cond.core :as b]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "01" "2023"))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; # Solution
;;
;; Load and parse our input
(def input (->> (io/resource "inputs/2023/day01.txt")
                slurp
                str/split-lines))
{:nextjournal.clerk/visibility {:code :show :result :show}}
(first input)

;; ## Part 1
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn- digits
  [s]
  (->> s
       (filter #(Character/isDigit %))
       (map #(Character/digit % 10))))

(defn- calibration-value
  [digits]
  (+ (* 10 (first digits))
     (last digits)))

(defn part-1
  [input]
  (->> input
       (map digits)
       (map calibration-value)
       (apply +)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
;;
;; We can use the same functions as Part 1 but we need to replace word forms of the
;; numbers in the input string.
(def word-replacements
  {"zero" "0"
   "one" "1"
   "two" "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six" "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"})

;; The unexpected surprise from the input is that we have to handle *all* of the replacements as they exist in the initial string, even if they overlap.
;;
;; For example: `oneight => 18`
;;
;; Since extra non-digit characters aren't an issue, we'll cheat by only advancing one character after replacement.
(defn replace-digit-words [s]
  (loop [processed []
         remaining s]
    (b/cond
      (empty? remaining) (str/join processed)

      :let [replacement (some (fn [[word digit]]
                              (when (str/starts-with? remaining word)
                                digit))
                            word-replacements)]
      replacement
      (recur (conj processed replacement) (subs remaining 1))

      :else
      (recur (conj processed (subs remaining 0 1)) (subs remaining 1)))))

(defn part-2
  [input]
  (->> input
       (map replace-digit-words)
       (map digits)
       (map calibration-value)
       (apply +)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 input)
