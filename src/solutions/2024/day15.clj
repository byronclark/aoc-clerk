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
(def tiles {\# :wall \. :floor \O :box \@ :robot \[ :box-left \] :box-right})
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

;; We'll see how this holds up, but boxes move the same way as robots so we can use the
;; same code.
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

;; We'll start by widening the map
(defn- widen
  [warehouse]
  (reduce (fn [widened location]
            (let [[x y] location
                  start-x (* 2 x)
                  tiles (case (grid/at warehouse location)
                          :floor [:floor :floor]
                          :wall [:wall :wall]
                          :robot [:robot :floor]
                          :box [:box-left :box-right])]
              (-> widened
                  (grid/set-at [start-x y] (first tiles))
                  (grid/set-at [(inc start-x) y] (second tiles)))))
          (grid/create (* 2 (:width warehouse)) (:height warehouse) :floor)
          (grid/all-locations warehouse)))


;; And now for the tricky part, moving boxes vertically. We have to figure out the
;; boundaries of the box and then check that both sides can move in the requested
;; direction, recursively pushing them around.
(defn- do-move-box-vertically
  [warehouse position move]
  {:pre [(#{:up :down} move)]}
  (bc/cond
    :let [tile (grid/at warehouse position)
          target-position (next-position position move)
          [x y] position
          [left right] (if (= :box-left tile)
                         [position [(inc x) y]]
                         [[(dec x) y] position])
          target-left (next-position left move)
          target-right (next-position right move)
          target-left-tile (grid/at warehouse target-left)
          target-right-tile (grid/at warehouse target-right)
          move-box (fn [next-warehouse]
                     [(-> next-warehouse
                          (swap-tiles left target-left)
                          (swap-tiles right target-right))
                      target-position])]

    (or (= :wall target-left-tile) (= :wall target-right-tile))
    [warehouse position]

    (and (= :floor target-left-tile) (= :floor target-right-tile))
    (move-box warehouse)

    ;; Box sitting completely on top
    (and (= :box-left target-left-tile) (= :box-right target-right-tile))
    (let [[after-attempt after-attempt-position] (do-move-box-vertically warehouse target-position move)]
      (if (= after-attempt-position target-position)
        [warehouse position]
        (move-box after-attempt)))

    ;; Box sitting on only the right side
    (= :floor target-left-tile)
    (let [[after-attempt after-attempt-position] (do-move-box-vertically warehouse target-right move)]
      (if (= after-attempt-position target-right)
        [warehouse position]
        (move-box after-attempt)))

    ;; Box sitting only on the left side
    (= :floor target-right-tile)
    (let [[after-attempt after-attempt-position] (do-move-box-vertically warehouse target-left move)]
      (if (= after-attempt-position target-left)
        [warehouse position]
        (move-box after-attempt)))

    ;; Two boxes sitting on top, need to ensure both can move
    :let [[after-left-warehouse after-left-position]
          (do-move-box-vertically warehouse target-left move)]

    ;; Left side couldn't move
    (= after-left-position target-left)
    [warehouse position]

    :let [[after-right-warehouse after-right-position]
          (do-move-box-vertically after-left-warehouse target-right move)]

    ;; Right side couldn't move
    (= after-right-position target-right)
    [warehouse position]

    :else
    (move-box after-right-warehouse)))

(defn- do-move-double-wide
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

    ;; Now we start moving boxes
    ;; Left and right are easy since we can treat either side of the box as a box.
    (#{:left :right} move)
    (let [[after-attempt target-after-attempt]
          (do-move-double-wide warehouse target-position move)]
      (if (= target-after-attempt target-position)
        [warehouse position]
        [(swap-tiles after-attempt position target-position) target-position]))

    :let [[after-attempt target-after-attempt]
          (do-move-box-vertically warehouse target-position move)]

    ;; No move possible
    (= target-after-attempt target-position)
    [warehouse position]

    :else
    [(swap-tiles after-attempt position target-position) target-position]))

;; ### Time for some experiments
{:nextjournal.clerk/visibility {:code :show :result :show}}
;; Here's what the tiny puzzle looks like in double wide mode
(str/split-lines (grid/display (widen (:warehouse tiny-puzzle)) tile->printable))

;; #### Nothing in the way
(let [warehouse (widen (:warehouse tiny-puzzle))
      [next-warehouse next-position] (do-move-double-wide warehouse (robot-position warehouse) :up)]
  {:position next-position
   :warehouse (str/split-lines (grid/display next-warehouse tile->printable))})

;; #### Wall in the way
(let [warehouse (widen (:warehouse tiny-puzzle))
      [next-warehouse next-position] (do-move-double-wide warehouse (robot-position warehouse) :left)]
  {:position next-position
   :warehouse (str/split-lines (grid/display next-warehouse tile->printable))})

;; #### Box in the way, no space to move box
(let [warehouse (widen (:warehouse tiny-puzzle))
      start-position [6 2]
      setup-warehouse (swap-tiles warehouse start-position (robot-position warehouse))
      [next-warehouse next-position] (do-move-double-wide setup-warehouse start-position :up)]
  {:position next-position
   :warehouse (str/split-lines (grid/display next-warehouse tile->printable))})

;; #### Box in the way horizontally, empty space before wall
(let [warehouse (widen (:warehouse tiny-puzzle))
      start-position [7 2]
      setup-warehouse (swap-tiles warehouse start-position (robot-position warehouse))
      [next-warehouse next-position] (do-move-double-wide setup-warehouse start-position :right)]
  {:position next-position
   :warehouse (str/split-lines (grid/display next-warehouse tile->printable))})

;; #### Multiple boxes in the way vertically, empty space before wall
(let [warehouse (widen (:warehouse tiny-puzzle))
      start-position [8 6]
      setup-warehouse (swap-tiles warehouse start-position (robot-position warehouse))
      [next-warehouse next-position] (do-move-double-wide setup-warehouse start-position :up)]
  {:position next-position
   :warehouse (str/split-lines (grid/display next-warehouse tile->printable))})

;; #### And then some boxes stacked oddly
(let [warehouse (grid/character-lines->grid "#########
#.......#
#.[][]..#
#..[]...#
#...@...#
#########" :convert tiles)
      [next-warehouse next-position]
      (do-move-double-wide warehouse (robot-position warehouse) :up)]
  {:position next-position
   :warehouse (str/split-lines (grid/display next-warehouse tile->printable))})

;; #### And then some boxes stacked oddly that can't move
(let [warehouse (grid/character-lines->grid "#########
#....[].#
#.[][]..#
#..[]...#
#...@...#
#########" :convert tiles)
      [next-warehouse next-position]
      (do-move-double-wide warehouse (robot-position warehouse) :up)]
  {:position next-position
   :warehouse (str/split-lines (grid/display next-warehouse tile->printable))})

;; ### And now that moving works, we can go back to the solution
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [{:keys [warehouse moves]}]
  (let [widened (widen warehouse)
        [after-moves _] (reduce (fn [[warehouse position] move]
                                  (do-move-double-wide warehouse position move))
                                [widened (robot-position widened)]
                                moves)]
    (->> after-moves
           grid/all-locations
           (filter #(= :box-left (grid/at after-moves %)))
           (map gps-coordinate)
           (apply +))))

{:nextjournal.clerk/visibility {:code :show :result :show}}
;; We also have a new tiny puzzle for part 2
(def part-2-tiny-puzzle (parse-puzzle "#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^"))

;; Which gives our answer for the tiny test input
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 part-2-tiny-puzzle)

;; And the medium test input
(part-2 test-puzzle)

;; And for the full input
(part-2 puzzle)
