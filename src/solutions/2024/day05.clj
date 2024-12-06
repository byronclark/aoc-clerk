^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day05
  {:nextjournal.clerk/toc true}
  (:require
   [better-cond.core :as b]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]
   [util :as u]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "05" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :hide}}

;; # Solution
;;
;; Load and parse our input
;;
;; For checking rules, it should make things easier to have a map from page number to all
;; the pages that must appear before it.
(defn parse-rules [lines]
  (->> lines
       (map (fn [line] (map parse-long (str/split line #"\|" 2))))
       (reduce (fn [rules [before after]]
                 (let [updated (conj (get rules after #{}) before)]
                   (assoc rules after updated)))
               {})))

(defn parse-update [line]
  (map parse-long (str/split line #",")))

(defn parse-input
  [raw-input]
  (let [[raw-rules raw-updates] (str/split raw-input #"\n\n" 2)]
    {:rules (->> raw-rules
                 str/split-lines
                 parse-rules)
     :updates (->> raw-updates
                   str/split-lines
                   (map parse-update))}))

{:nextjournal.clerk/visibility {:code :show :result :show}}
(def input (parse-input (slurp (io/resource "inputs/2024/day05.txt"))))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; For each page in the update we have to ensure that none of the pages required to appear
;; before the page actually show up after.
(defn valid-update?
  [rules update]
  (loop [[page & after] update]
    (cond
      (empty? after)
      true

      (seq (set/intersection (get rules page #{})
                             (set after)))
      false

      :else
      (recur after))))

(defn middle-page
  [update]
  (nth update (quot (count update) 2)))

(defn part-1
  [{:keys [rules updates]}]
  (->> updates
       (filter (partial valid-update? rules))
       (map middle-page)
       (apply +)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
;;
;; And now we get a little more tricky. Instead of just finding them, we get to fix the
;; ordering based on the rules we have.
;;
;; I'm a little shocked that we can use `first` on the intersection to rotate these as
;; it's not guaranteed to be any specific ordering. But since we're not counting it as
;; reordered until there are no conflicts, it's likely we do multiple shuffles there.
(defn reorder
  [rules update]
  (loop [reordered []
         [page & after] update]
    (b/cond
      (empty? after)
      (conj reordered page)

      :let [conflicts (set/intersection (get rules page #{})
                                        (set after))]

      (empty? conflicts)
      (recur (conj reordered page) after)

      :else
      (let [to-move (first conflicts)]
        (recur reordered
               (concat [(first conflicts) page]
                       (remove #(= to-move %) after)))))))

;; At least we can use our Part 1 code to filter out the valid updates.
(defn part-2
  [{:keys [rules updates]}]
  (->> updates
       (remove (partial valid-update? rules))
       (map (partial reorder rules))
       (map middle-page)
       (apply +)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 input)
