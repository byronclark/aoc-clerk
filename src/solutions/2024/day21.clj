^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day21
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [clojure.math.combinatorics :as combo]
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]
   [solutions.2024.helpers.pathfinding :refer [a*-with-path]]
   [util :as u]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "21" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; Load and parse our input
(defn parse-code
  [s]
  (map #(if (= \A %) :enter %) s))

(def input (->> (io/resource "inputs/2024/day21.txt")
                slurp
                str/split-lines
                (map parse-code)))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; The biggest things we need here are maps that tell how to get from one
;; location to another on each type of keypad in the shortest number of steps.
;; I'm guessing it will be important to have alternate paths if they are the
;; same length so we can avoid the empty space.
;;
;; It's easier to do on the d-pad.
(def paths-d-pad
  {:left  {:down  [[:right]]
           :right [[:right :right]]
           :up    [[:right :up]]
           :enter [[:right :right :up]
                   [:right :up :right]]}
   :down  {:left  [[:left]]
           :right [[:right]]
           :up    [[:up]]
           :enter [[:right :up]
                   [:up :right]]}
   :right {:left  [[:left :left]]
           :down  [[:left]]
           :up    [[:left :up]
                   [:up :left]]
           :enter [[:up]]}
   :up    {:left  [[:down :left]]
           :down  [[:down]]
           :right [[:down :right]
                   [:right :down]]
           :enter [[:right]]}
   :enter {:left  [[:left :down :left]
                   [:down :left :left]]
           :down  [[:left :down]
                   [:down :left]]
           :right [[:down]]
           :up    [[:left]]}})

;; That was tedious... I really don't want to do this by hand for the full
;; 10-key pad.
;;
;; Time to build a graph and break out our pathfinding algorithm. But then I discovered that my version of A* with the path isn't exhaustive 🤦. This is small enough that we can be inefficient.
(def connections-10key
  {\0     [{:target \2 :direction :up}
           {:target :enter :direction :right}]
   :enter [{:target \0 :direction :left}
           {:target \3 :direction :up}]
   \1     [{:target \2 :direction :right}
           {:target \4 :direction :up}]
   \2     [{:target \1 :direction :left}
           {:target \0 :direction :down}
           {:target \3 :direction :right}
           {:target \5 :direction :up}]
   \3     [{:target \2 :direction :left}
           {:target :enter :direction :down}
           {:target \6 :direction :up}]
   \4     [{:target \1 :direction :down}
           {:target \5 :direction :right}
           {:target \7 :direction :up}]
   \5     [{:target \4 :direction :left}
           {:target \2 :direction :down}
           {:target \6 :direction :right}
           {:target \8 :direction :up}]
   \6     [{:target \5 :direction :left}
           {:target \3 :direction :down}
           {:target \9 :direction :up}]
   \7     [{:target \4 :direction :down}
           {:target \8 :direction :right}]
   \8     [{:target \7 :direction :left}
           {:target \5 :direction :down}
           {:target \9 :direction :right}]
   \9     [{:target \8 :direction :left}
           {:target \6 :direction :down}]})


(defn all-paths
  [connections start end]
  (letfn [(inner [path current]
            (let [next-path (conj path current)
                  visited (set (keep :target path))]
              (if (= (:target current) end)
                [next-path]
                (->> (get connections (:target current))
                     (mapcat (fn [connection]
                               (when-not (contains? visited (:target connection))
                                 (inner next-path connection))))
                     (keep identity)))))]
    (inner [] {:target start})))

{:nextjournal.clerk/visibility {:code :show :result :show}}
(def paths-10key
  (->> (combo/selections (keys connections-10key) 2)
       (remove #(= (first %) (second %)))
       (reduce (fn [found [start end]]
                 (assoc-in found [start end]
                           (->> (all-paths connections-10key start end)
                                (map (partial keep :direction)))))
               {})))

;; The idea of robots is neat, but for writing the solution, a robot is just a
;; keypad with a current location that knows how to move. Not sure how that
;; really helps solving this.
;;
;; The part that's throwing me here is that we have to find the *shortest* code
;; across multiple layers. There's a high probability I'll be wrong, but I'm
;; going to assume that if we pick the shortest path on the last pad, then one
;; of those paths will be the shortest on each of the pads going back.
;;
;; Now that I've written that, I think we have a different problem to solve.
;; This is pathfinding repeated multiple times *if* we can still assume shortest
;; path on one layer will lead to shortest path on the next. Lets find out.

(defn shortest
  "Given a seq of routes, return all of the shortest by length."
  [routes]
  (let [by-length (group-by count routes)
        min-length (apply min (keys by-length))]
    (get by-length min-length)))

(defn routes
  "Given possible paths between all locations, return the shortest routes from start to end."
  [paths start end]
  (get-in paths [start end] [[]]))

(defn walks
  "Given possible paths between locations, return the walks that visit all locations in steps.

  Adds an `:enter` between the moves for each location."
  [paths steps]
  (->> steps
       (partition 2 1)
       (reduce (fn [found-paths [start end]]
                 (let [next-paths (routes paths start end)]
                   (for [path (if (seq found-paths) found-paths [[]])
                         next-path next-paths]
                     (concat path next-path [:enter]))))
               nil)))

(defn fastest-walk
  "It's like walks when we don't care what the shortest route is, just how long it is."
  [paths steps]
  (->> steps
       (partition 2 1)
       (map (fn [[start end]]
              (->> (routes paths start end)
                   (map count)
                   (apply min)
                   inc))) ;inc accounts for the :enter we have to press
       (apply +)))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn enter-code
  "Given a code to be typed on the numeric keypad, returns the shortest sequence to enter that code."
  [code]
  (let [shortest-robot1 (->> code
                             (cons :enter)
                             (walks paths-10key)
                             shortest)
        shortest-robot2 (->> shortest-robot1
                             (map #(cons :enter %))
                             (mapcat #(walks paths-d-pad %))
                             shortest)]

    ;; Luckily for the last d-pad we don't need to know the exact sequence, only
    ;; how long it is.
    (->> shortest-robot2
         (map #(cons :enter %))
         (map #(fastest-walk paths-d-pad %))
         (apply min))))

(defn code->long
  [code]
  (->> code
       (filter #(and (char? %)
                     (Character/isDigit %)))
       (map #(Character/digit % 10))
       (reduce (fn [acc digit]
                 (+ digit (* 10 acc)))
               0)))

(defn part-1
  [codes]
  (->> codes
       (map (fn [code]
              (* (enter-code code)
                 (code->long code))))
       (apply +)))

;; Which gives our answer with a test input
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 [(parse-code "029A")
         (parse-code "980A")
         (parse-code "179A")
         (parse-code "456A")
         (parse-code "379A")])

;; And the full set of codes
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
(defn part-2
  [input]
  (println "Part 2"))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 input)
