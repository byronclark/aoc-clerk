^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day13
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [util :as u]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]
   [better-cond.core :as bc]
   [clojure.math.combinatorics :as combo]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "13" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :hide}}

;; # Solution
;;
;; Load and parse our input
;;
;; That's neat that the button definitions include `+` to make the regex more fun.
(defn- parse-button [line]
  (->> line
       (re-find #"Button [AB]: X\+(\d+), Y\+(\d+)")
       (drop 1)
       (map parse-long)))

(defn- parse-prize [line]
  (->> line
       (re-find #"Prize: X=(\d+), Y=(\d+)")
       (drop 1)
       (map parse-long)))

(defn- parse-claw-machine [machine]
  (let [lines (str/split-lines machine)]
    {:a (parse-button (nth lines 0))
     :b (parse-button (nth lines 1))
     :prize (parse-prize (nth lines 2))}))

{:nextjournal.clerk/visibility {:code :show :result :show}}
(def input (-> (io/resource "inputs/2024/day13.txt")
               slurp
               (str/split #"\n\n")
               (#(map parse-claw-machine %))))

;; And the smaller test input
(def test-input (-> "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"
                    (str/split #"\n\n")
                    (#(map parse-claw-machine %))))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; First attempt was a recursive walk, but that wasn't nearly as efficient as I hoped. Given the constraint on how many times each button can be pushed, I think it's going to be enough to generate all the possible press combinations (* 100 100) and see if any of them match.

(defn- presses->location
  [{:keys [a b]} [presses-a presses-b]]
  (let [[a-x a-y] a
        [b-x b-y] b]
    [(+ (* presses-a a-x)
        (* presses-b b-x))
     (+ (* presses-a a-y)
        (* presses-b b-y))]))

(defn- cost
  [[presses-a presses-b]]
  (+ (* 3 presses-a) presses-b))

(defn- cost-to-win
  [max-pushes {:keys [a b prize] :as machine}]
  (let [[prize-x prize-y] prize
        max-pushes-a (min max-pushes (quot prize-x (first a)) (quot prize-y (second a)))
        max-pushes-b (min max-pushes (quot prize-x (first b)) (quot prize-y (second b)))
        costs (->> (combo/cartesian-product (range (inc max-pushes-a))
                                            (range (inc max-pushes-b)))
                   (filter #(let [[x y] (presses->location machine %)]
                              (and (= x prize-x)
                                   (= y prize-y))))
                   (map cost))]
    (if (seq costs)
      (apply min costs)
      0)))

(defn part-1
  [machines]
  (->> machines
       (keep (partial cost-to-win 100))
       (apply +)))

;; Which gives our answer with the test input
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
