^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day07
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [util :as u]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "07" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :hide}}

;; # Solution
;;
;; Load and parse our input
(defn parse-equation [line]
  (let [parts (->> (str/split line #"[: ]+")
                   (map parse-long))]
    {:result (first parts)
     :values (vec (rest parts))}))

(defn parse-input [raw-input]
  (->> raw-input
       str/split-lines
       (map parse-equation)))

(def input (-> (io/resource "inputs/2024/day07.txt")
               slurp
               parse-input))

;; We'll want to test with the smaller input from the instructions first.
(def test-input (parse-input "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"))

;; Some sample equations from the full input
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(take 5 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; Let's start simple by testing all possible operator combinations to see if one works.
(defn compute
  [values operators]
  {:pre [(= (inc (count operators)) (count values))]}
  (->> (map vector (rest values) operators)
       (reduce (fn [acc [value operator]]
                 (operator acc value))
               (first values))))

(defn possible?
  [operators {:keys [result values]}]
  (->> (combo/selections operators (dec (count values)))
       (some #(= result (compute values %)))))

(defn part-1
  [equations]
  (->> equations
       (pmap (fn [equation]
               (when (possible? [+ *] equation)
                 (:result equation))))
       (keep identity)
       (apply +)))

;; Which gives our test answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 test-input)
;; And the final answer
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
;;
;; This appears to be a repeat of Part 1 with a new operator: `||`. Let's implement the
;; operator and we'll see if we need any other tricks.
(defn ||
  [a b]
  (parse-long (str a b)))

;; It's more expensive to check with three operators, so let's only do the check for
;; equations that don't work with the original two operators.
(defn part-2
  [equations]
  (let [{with-two true check-three false}
        (->> equations
             (pmap #(vector (boolean (possible? [+ *] %)) %))
             (group-by first))

        with-three (->> check-three
                        (map second)
                        (pmap #(when (possible? [+ * ||] %) %))
                        (keep identity))]
    (->> with-two
         (map second)
         (concat with-three)
         (map :result)
         (apply +))))

;; Which gives our test answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 test-input)
;; And the final answer
(part-2 input)
