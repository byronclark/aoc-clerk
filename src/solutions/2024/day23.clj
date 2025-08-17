^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day23
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]
   [util :as u]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "23" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; Load and parse our input into a map that contains all the connections starting at a computer.
(defn- input->connections
  [raw-input]
  (->> raw-input
       str/split-lines
       (map #(str/split % #"-"))
       (reduce (fn [connections [computer1 computer2]]
                 (-> connections
                     (update computer1 (fnil conj #{}) computer2)
                     (update computer2 (fnil conj #{}) computer1)))
               {})))

(def input (->> (io/resource "inputs/2024/day23.txt")
                slurp
                input->connections))

;; And as usual we have some test input
(def test-input (input->connections "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn"))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; Our goal here is to find groups of three computers that all connect to each
;; other. Trimming down to groups that include a computer that starts with `t`
;; is a simpler problem.
;;
;; Let's start with simple and iterate through. To see if computer0 is in a
;; group of three we find all the computers that are connected directly to
;; computer0 and then check all the other computers to see if there's a computer
;; that connects to both. This is $O(n^2)$ so I expect we'll have to do better
;; at some point in the challenge.
;;
;; The simplification is that we don't have to check all the sets for
;; membership, only find those nodes that are in connections to computer0 and
;; computer1.
(defn groups-of-three
  [connections]
  (set (for [[computer0 computer0-connections] connections
             computer1 computer0-connections
             :let [computer1-connections (get connections computer1)]
             computer2 (set/difference
                        (set/intersection computer0-connections computer1-connections)
                        #{computer0 computer1})]
         #{computer0 computer1 computer2})))

(defn part-1
  [connections]
  (->> connections
       groups-of-three
       (filter (fn [computers] (some #(= \t (first %)) computers)))
       count))

;; Which gives our answer for the test input
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 test-input)

;; And with the full input
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
;;
;; Now we have to find the largest interconnected set of computers. I'm glad I
;; didn't spend too much time optimizing part 1 since this feels like a very
;; different problem.
;;
;;

(defn part-2
  [connections]
  (println "Part 2"))

;; Which gives our answer for the test input
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 input)

;; And with the full input
(part-2 input)
