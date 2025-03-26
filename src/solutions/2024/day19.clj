^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day19
  {:nextjournal.clerk/toc true}
  (:require
   [better-cond.core :as bc]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]
   [util :as u]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "19" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :hide}}

;; # Solution
;;
;; Load and parse our input
(defn- parse-input [input]
  (let [[raw-towels raw-designs] (str/split input #"\n\n")]
    {:towels (str/split raw-towels #", ")
     :designs (str/split-lines raw-designs)}))

{:nextjournal.clerk/visibility {:code :show :result :show}}
(def input (-> (io/resource "inputs/2024/day19.txt")
                slurp
                parse-input))

;; And a smaller test input
(def test-input (parse-input "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb"))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; We need to find out if we can construct a specific design with an infinite supply of towel patterns we were provided.
(defn- buildable?
  [towels pattern]
  (if (str/blank? pattern)
    true
    (some (fn [towel] (and (str/starts-with? pattern towel)
                           (buildable? towels (subs pattern (count towel)))))
          towels)))

(defn part-1
  [{:keys [towels designs]}]
  (->> designs
       (filter (partial buildable? towels))
       count))

;; Which gives our answer for the test input
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 test-input)

;; And our full input
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
;;
;; And now we need to find all the ways it's possible to construct them. Luckily I don't think we need to know the exact patterns so we don't have to carry a full path.
;;
;; But we will need some memoization to deal with the size of the full input
(defn- ways-to-build
  [memory towels pattern]
  (let [inner (fn inner [p]
                (bc/cond
                  (str/blank? p)
                  1

                  :let [found (get @memory p)]

                  found
                  found

                  :else
                  (let [builds (->> towels
                                    (keep (fn [towel] (when (str/starts-with? p towel)
                                                        (inner (subs p (count towel))))))
                                    (apply +))]
                    (swap! memory assoc p builds)
                    builds)))]
    (inner pattern)))

(defn part-2
  [{:keys [towels designs]}]
  (let [found (atom {})]
    (->> designs
         (map (partial ways-to-build found towels))
         (apply +))))

;; Which gives our answer for the test input
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 test-input)

;; And our full input
(part-2 input)
