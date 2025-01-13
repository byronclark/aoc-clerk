(ns solutions.2024.helpers.grid
  "Build and operate on two dimensional grids."
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.string :as str]
   [solutions.2024.helpers.grid :as grid]))

(defn char->digit
  [character]
  (Character/digit character 10))

(defn character-lines->grid
  "Convert lines of characters into a map of height, width, values.

  Values is a vector of all values organized by row.

  The optional `convert` is an arity one function that converts input
  character to desired value."
  ([input & {:keys [convert] :or {convert identity}}]
   (let [rows (str/split-lines input)]
     {:height (count rows)
      :width (count (first rows))
      :values (->> rows
                   (mapcat (fn [row]
                             (map convert row)))
                   vec)})))

(defn valid-location?
  [grid [x y]]
  (and (>= x 0)
       (< x (:width grid))
       (>= y 0)
       (< y (:height grid))))

(defn- location->index
  [grid [x y]]
  (+ (* y (:width grid))
     x))

(defn create
  ([width height]
   (create width height 0))
  ([width height default]
   {:height height
    :width width
    :values (vec (repeat (* width height) default))}))

(defn at
  [grid location]
  (when (valid-location? grid location)
    (nth (:values grid) (location->index grid location))))

(defn display
  "Returns a multiple line string representation of the grid.

  Values can be transformed with display-fn."
  ([grid]
   (display grid identity))
  ([grid display-fn]
   (str/join
    "\n"
    (map (fn [y]
           (str/join
            ""
            (map (fn [x]
                   (display-fn (at grid [x y])))
                 (range (:width grid)))))
         (range (:height grid))))))

(defn set-at
  [grid location value]
  (if (valid-location? grid location)
    (assoc-in grid [:values (location->index grid location)] value)
    grid))

(defn update-at
  [grid location f & args]
  (if (valid-location? grid location)
    (apply update-in grid [:values (location->index grid location)] f args)
    grid))

(defn adjacent-locations
  "Valid locations either horizontally or vertically adjacent to location."
  [grid location]
  (let [[x y] location]
    (->> [[(dec x) y] [x (dec y)] [(inc x) y] [x (inc y)]]
         (filter #(grid/valid-location? grid %)))))

(defn all-locations
  [grid]
  (combo/cartesian-product (range (:width grid))
                           (range (:height grid))))

(defn find-value
  [grid value]
  (filter #(= value (at grid %)) (all-locations grid)))
