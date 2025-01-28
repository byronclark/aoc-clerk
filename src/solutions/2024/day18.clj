^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day18
  {:nextjournal.clerk/toc true}
  (:require
   [better-cond.core :as bc]
   [clojure.java.io :as io]
   [clojure.math :as math]
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]
   [solutions.2024.helpers.grid :as grid]
   [solutions.2024.helpers.pathfinding :refer [a*]]
   [util :as u]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "18" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; Load and parse our input
(defn ->location [line]
  (mapv parse-long (str/split line #"," 2)))

(def input {:side 71
            :locations (->> (io/resource "inputs/2024/day18.txt")
                            slurp
                            str/split-lines
                            (map ->location))})

;; And our smaller test input
(def test-input {:side 7
                 :locations (map ->location (str/split "5,4 4,2 4,5 3,0 2,1 6,3 2,4 1,5 0,6 3,3 2,6 5,1 1,2 5,5 2,5 6,5 1,4 0,4 6,4 1,1 6,1 1,0 0,5 1,6 2,0" #" "))})

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; In case we need to display the grid in a readable fashion.
(def ->display {:valid \. :corrupt \#})

;; Then we can mark the corrupted locations in the memory grid and do some path-finding. Which means it's time to move our A* implementation from day 16 to a shared namespace and use it again.
(defn path-length
  [side locations]
  (let [memory (grid/create side side :valid)
        corrupted (reduce (fn [memory location]
                                 (grid/set-at memory location :corrupt))
                          memory
                          locations)]
    (a* [0 0] {:connections-fn (fn [location]
                                 (->> location
                                      (grid/adjacent-locations corrupted)
                                      (filter #(= :valid (grid/at corrupted %)))))
               :target-fn identity
               :cost-fn (constantly 1)  ;Simple because connections-fn filters valid connections

               :done?-fn #(= [(dec side) (dec side)] %)})))

(defn part-1
  [input cutoff]
  (let [{:keys [side locations]} input]
    (path-length side (take cutoff locations))))

;; Which gives our answer for the test input
{:nextjournal.clerk/visibility {:code :show :result :show}}
(part-1 test-input 12)

;; And the full input
(part-1 input 1024)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
;;
;; I'm 100% sure there's a more efficient way to do this, but for now we're going to
;; binary search for where in the list we stop being able to exit the maze and wait. With
;; the hope that we could actually make the A* implementation efficient with a real
;; priority queue.
(defn part-2
  [input]
  (let [{:keys [side locations]} input]
    (loop [bottom 0
           top (count locations)]
      (bc/cond
        :let [checking (+ (quot (- top bottom) 2) bottom)]
        (= bottom checking)
        [checking (nth locations bottom)]

        :let [path (path-length side (take checking locations))]

        (nil? path)                     ;we know the cutoff is below here
        (recur bottom checking)

        :else                           ;or above here
        (recur checking top)))))

;; Which gives our answer for the test input
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 test-input)

;; And the full input
(part-2 input)

;; I guess I shouldn't be too shocked that a binary search with A* at each step was fast enough. The total number of A* runs is $log_{2} N$ where N is the number of memory locations in the input. My input only had 3450 locations so worst case is 12 A* runs.
