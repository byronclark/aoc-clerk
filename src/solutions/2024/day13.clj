^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day13
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [util :as u]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]
   [better-cond.core :as bc]
   [clojure.core.matrix :as m]
   [clojure.core.matrix.linear :as lin]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
^::clerk/no-cache
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
;; First attempt was a recursive walk, but that wasn't nearly as efficient as I hoped.
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

;; To make this more efficient we really need to find a reasonable range to test.
;;
;; Finding the maximum number of pushes for each button is straightforward: find how many
;; times we can push the button before it moves past in either direction. Finding the
;; minimum number of presses is a bit more tricky because it really can be zero. The
;; search space is small enough with the max-pushes limit for part 1 that we'll skip
;; finding the minimum to start with.
(defn- cost-to-win
  [max-pushes {:keys [a b prize] :as machine}]
  (let [[prize-x prize-y] prize
        [a-x a-y] a
        [b-x b-y] b
        max-pushes-a (min (quot prize-x a-x) (quot prize-y a-y))
        max-pushes-b (min (quot prize-x b-x) (quot prize-y b-y))

        costs (->> (combo/cartesian-product (range (min max-pushes max-pushes-a))
                                            (range (min max-pushes max-pushes-b)))
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
;;
;; There's no way we can iterate from 0 on this one. At least I'm not willing to wait for that.
;;
;; I'm going to try something a little different:
;; 1. Find how many B presses it takes to get past the prize in any direction
;; 2. Until we hit the prize or fall off the edge:
;;    1. Remove a B press
;;    2. Add an A press if possible
;;    3. Win?
;;
;; But that doesn't work because it's still a massive number of iterations to try.
(defn- cost-to-win-iterate-down
  [{:keys [a b prize]}]
  (let [[prize-x prize-y] prize
        [a-x a-y] a
        [b-x b-y] b
        start-b (inc (min (quot prize-x b-x) (quot prize-y b-y)))]
    (loop [presses-a 0
           presses-b start-b]
      (bc/cond
        :let [x (+ (* presses-a a-x) (* presses-b b-x))
              y (+ (* presses-a a-y) (* presses-b b-y))]

        (or (neg? x) (neg? y))
        nil

        (and (= x prize-x) (= y prize-y))
        (cost [presses-a presses-b])

        (or (>= x prize-x) (>= y prize-y))
        (recur presses-a (dec presses-b))

        :else
        (recur (inc presses-a) presses-b)))))

;; At this point it's probably time to return to basic math. We have two equations:
;;
;; $prize_x = n_a * a_x + n_b * b_x$
;;
;; $prize_y = n_a * a_y + n_b * b_y$
;;
;; Two equations, two unknowns.
;;
;; Let's assume there's only one solution for each and it's the lowest cost. 🤞
(m/set-current-implementation :vectorz)

;; We only care about integer solutions, but linear algebra libraries usually operate on
;; floats so we get to check for almost integers.
;;
;; To be fair, it's probably overkill to use a full library for solving a simple set of
;; equations like this, but I'm hopeful it'll be useful in other days.
(defn- integer-solution?
  "Returns true if all elements are close enough to an integers"
  [a & {:keys [epsilon] :or {epsilon 1e-3}}]
  (-> a
      m/round
      (m/sub a)
      m/abs
      (m/lt epsilon)
      m/zero-count
      zero?))

(defn- cost-to-win-solve
  [{:keys [a b prize]}]
  (let [[prize-x prize-y] prize
        [a-x a-y] a
        [b-x b-y] b
        solution (lin/solve [[a-x b-x]
                             [a-y b-y]]
                            [prize-x prize-y])]
    (when (integer-solution? solution)
      (->> solution
           m/round
           (map long)
           cost))))

(defn- update-prize-location [n machine]
  (update machine :prize #(map (partial + n) %)))

(defn part-2
  [machines]
  (->> machines
       (map (partial update-prize-location 10000000000000))
       (keep cost-to-win-solve)
       (apply +)))

;; Which gives our answer with the test input
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 test-input)

;; And the full input
(part-2 input)
