^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day15
  {:nextjournal.clerk/toc true}
  (:require
   [better-cond.core :as bc]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]
   [solutions.2024.helpers.grid :as grid]
   [util :as u]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "15" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :hide}}

;; # Solution
;;
;; Load and parse our input
(def tiles {\# :wall \. :floor \O :box \@ :robot})
(def tile->printable (set/map-invert tiles))

(defn- parse-puzzle [input]
  (let [[warehouse moves] (str/split input #"\n\n" 2)]
    {:warehouse (grid/character-lines->grid warehouse :convert tiles)
     :moves (keep {\< :left \^ :up \> :right \v :down} moves)}))

{:nextjournal.clerk/visibility {:code :show :result :show}}
(def puzzle (->> (io/resource "inputs/2024/day15.txt")
                 slurp
                 parse-puzzle))

;; A medium size test puzzle
(def test-puzzle (parse-puzzle "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"))

;; And a tiny test puzzle
(def tiny-puzzle (parse-puzzle "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<"))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; At least we know how to find the robot...
(defn- robot-position [warehouse]
  (->> warehouse
       grid/all-locations
       (filter #(= :robot (grid/at warehouse %)))
       first))

;; This is going to go in a loop, but we might as well encapsulate the movement outside of the loop.
(defn- next-position [[x y] move]
  (case move
    :left [(dec x) y]
    :up [x (dec y)]
    :right [(inc x) y]
    :down [x (inc y)]))

(defn- swap-tiles
  [warehouse location-1 location-2]
  (let [tile-1 (grid/at warehouse location-1)
        tile-2 (grid/at warehouse location-2)]
    (-> warehouse
        (grid/set-at location-1 tile-2)
        (grid/set-at location-2 tile-1))))

;; We'll see how this holds up, but boxes move the same way as robots so we can use the same code.
(defn- do-move
  "If possible, perform move on the tile at position.

  Returns the updated warehouse and new position of the tile."
  [warehouse position move]
  (bc/cond
    :let [target-position (next-position position move)
          target-tile (grid/at warehouse target-position)]

    (= :wall target-tile)
    [warehouse position]

    (= :floor target-tile)
    (let [next-warehouse (swap-tiles warehouse position target-position)]
      [next-warehouse target-position])

    :let [[after-attempt target-after-attempt]
          (do-move warehouse target-position move)]

    ;; No move possible
    (= target-after-attempt target-position)
    [warehouse position]

    :else
    [(swap-tiles after-attempt position target-position) target-position]))

;; ### Time for some experiments
{:nextjournal.clerk/visibility {:code :show :result :show}}
;; #### Nothing in the way
(let [warehouse (:warehouse tiny-puzzle)
      [next-warehouse next-position] (do-move warehouse (robot-position warehouse) :up)]
  {:position next-position
   :warehouse (str/split-lines (grid/display next-warehouse tile->printable))})

;; #### Wall in the way
(let [warehouse (:warehouse tiny-puzzle)
      [next-warehouse next-position] (do-move warehouse (robot-position warehouse) :left)]
  {:position next-position
   :warehouse (str/split-lines (grid/display next-warehouse tile->printable))})

;; #### Box in the way, no space to move box
(let [warehouse (:warehouse tiny-puzzle)
      start-position [3 2]
      setup-warehouse (swap-tiles warehouse start-position (robot-position warehouse))
      [next-warehouse next-position] (do-move setup-warehouse start-position :up)]
  {:position next-position
   :warehouse (str/split-lines (grid/display next-warehouse tile->printable))})

;; #### Multiple boxes in the way, empty space before wall
(let [warehouse (:warehouse tiny-puzzle)
      start-position [4 6]
      setup-warehouse (swap-tiles warehouse start-position (robot-position warehouse))
      [next-warehouse next-position] (do-move setup-warehouse start-position :up)]
  {:position next-position
   :warehouse (str/split-lines (grid/display next-warehouse tile->printable))})

;; ### And now that moving works, we can go back to the solution
{:nextjournal.clerk/visibility {:code :show :result :hide}}

(defn- gps-coordinate [[x y]]
  (+ x (* 100 y)))

(defn part-1
  [{:keys [warehouse moves]}]
  (let [[after-moves _] (reduce (fn [[warehouse position] move]
                                  (do-move warehouse position move))
                                [warehouse (robot-position warehouse)]
                                moves)]
    (->> after-moves
         grid/all-locations
         (filter #(= :box (grid/at after-moves %)))
         (map gps-coordinate)
         (apply +))))

;; Which gives our answer for the tiny test input
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 tiny-puzzle)

;; And the medium test input
(part-1 test-puzzle)

;; And for the full input
(part-1 puzzle)


{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
(defn part-2
  [input]
  (println "Part 2"))

;; Which gives our answer for the test input
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 tiny-puzzle)
