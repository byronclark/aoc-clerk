^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day14
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]
   [solutions.2024.helpers.grid :as grid]
   [util :as u]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "14" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :hide}}

;; # Solution
;;
;; Load and parse our input
(defn- parse-robot [line]
  (let [[pos-x pos-y v-x v-y] (->> line
                                   (re-find #"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)")
                                   (drop 1)
                                   (map parse-long))]
    {:position [pos-x pos-y]
     :velocity [v-x v-y]}))

{:nextjournal.clerk/visibility {:code :show :result :show}}
(def input {:floor {:width 101
                    :height 103}
            :robots (->> (io/resource "inputs/2024/day14.txt")
                         slurp
                         str/split-lines
                         (map parse-robot))})

;; And our test input with a different size grid.
(def test-input {:floor {:width 11
                         :height 7}
                 :robots (->> "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"
                              str/split-lines
                              (map parse-robot))})

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; Given the teleportation and ability to occupy the same space as another robot,
;; calculating the position of a robot after a number of seconds is a simple formula.
(defn position-at
  [{:keys [width height]} {:keys [position velocity]} seconds]
  (let [[x y] position
        [v-x v-y] velocity]
    [(mod (+ x (* v-x seconds))
          width)
     (mod (+ y (* v-y seconds))
          height)]))

;; And then we can find the quadrant, discarding anything that's on the line.
(defn quadrant
  [mid-x mid-y [x y]]
  (cond
    (and (< x mid-x) (< y mid-y))
    :top-left

    (and (> x mid-x) (< y mid-y))
    :top-right

    (and (> x mid-x) (> y mid-y))
    :bottom-right

    (and (< x mid-x) (> y mid-y))
    :bottom-left))

(defn part-1
  [{:keys [floor robots]}]
  (let [mid-x (quot (:width floor) 2)
        mid-y (quot (:height floor) 2)]
    (->> robots
         (map #(position-at floor % 100))
         (group-by (partial quadrant mid-x mid-y))
         (keep (fn [[quadrant robots]] (when quadrant (count robots))))
         (apply *))))

;; Which gives our answer for the test-input
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 test-input)

;; And for the full input
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
;;
;; A 🎄, eh?
;;
;; For the first example we have 12 robots on an 11x7 grid. Here's how we could arrange them to be a tree.
;; ```
;; .....X.....
;; ....X.X....
;; ...X...X...
;; ..X.....X..
;; .X.......X.
;; X.........X
;; .....X.....
;; ```
;;
;; For the full input we have 500 robots on a 101x103 grid. With two trees on each row except the top and bottom, we'd end up with 406 robots in use....
;;
;; I was hopeful that the real result would have a bunch of robots on the same row ow column and I could look for those, but that didn't look promising. I think there's some noise in the picture.
;;
;; I've looked through enough of these, and I'm not sure there is one for the test input, without finding any evidence of a tree that I went looking for a hint. Apparently there's something about times where there are no robots in duplicate positions.
(defn- duplicates [positions]
  (- (count positions) (count (set positions))))

(defn part-2
  [{:keys [floor robots]}]
  (some (fn [t]
          (let [positions (map #(position-at floor % t) robots)]
            (when (zero? (duplicates positions))
              {:t t :positions positions})))
        (range)))


{:nextjournal.clerk/visibility {:code :hide :result :show}}
(let [{:keys [t positions]} (part-2 input)]
  (clerk/plotly {:data [{:x (map first positions)
                         :y (map (comp - second) positions)
                         :type "scatter"
                         :mode "markers"
                         :marker {:color "green"}}]
                 :layout {:title {:text (str "Robot positions after " t " seconds")}
                          :xaxis {:visible false}
                          :yaxis {:visible false}
                          :paper_bgcolor "transparent"
                          :plot_bgcolor "transparent"}
                 :config {:displayModeBar false
                          :displayLogo false}}))
