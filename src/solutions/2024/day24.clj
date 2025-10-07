^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day24
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [util :as u]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "24" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :hide}}

;; # Solution
;;
;; Load and parse our input
(defn- ->initial-value
  [line]
  (let [[node value] (str/split line #": ")]
    [node {:type :input
           :value (= "1" value)}]))

(defn- ->gate
  [line]
  (let [[_ input0 op input1 output] (re-matches #"(\S+) (XOR|AND|OR) (\S+) -> (\S+)" line)]
    [output {:type :gate
             :operation (case op
                          "XOR" :xor
                          "AND" :and
                          "OR" :or)
             :inputs [input0 input1]}]))

(defn parse-input
  [raw-input]
  (let [parts (str/split raw-input #"\n\n")
        initial-values (->> parts
                            first
                            str/split-lines
                            (map ->initial-value)
                            (into {}))]
    (reduce (fn [circuit gate-desc]
              (let [[node gate] (->gate gate-desc)]
                (assoc circuit node gate)))
            initial-values
            (str/split-lines (second parts)))))

{:nextjournal.clerk/visibility {:code :show :result :show}}
(def input (->> (io/resource "inputs/2024/day24.txt")
                slurp
                parse-input))

;; The mini test input
(def mini-input (parse-input "x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02"))

;; And the larger test input
(def test-input (parse-input "x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj"))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; We need to "simulate" the circuit. For this we'll rely on Clojure's delay functionality so we can turn all of the nodes into a function that will only be run the first time it's needed.
(def gate-ops
  {:and (fn [a b] (and a b))
   :or (fn [a b] (or a b))
   :xor (fn [a b] (and (not= a b)
                       (or a b)))})
(defn- ->simulator
  [circuit]
  (->> circuit
       (map (fn [[node-name contents]]
              [node-name
               (case (:type contents)
                 :input (fn [_] (delay (:value contents)))
                 :gate (let [{:keys [inputs operation]} contents]
                         (fn [simulator]
                           (delay (->> inputs
                                       (map (fn [input] (deref ((get simulator input) simulator))))
                                       (apply (operation gate-ops)))))))]))
       (into {})))

(defn part-1
  [circuit]
  (let [simulator (->simulator circuit)
        result-nodes (reverse (sort (filter #(str/starts-with? % "z") (keys simulator))))]
    (->> result-nodes
         (map (fn [node] ((get simulator node) simulator)))
         (map #(if @% 1 0))
         (reduce (fn [acc bit]
                   (bit-or (bit-shift-left acc 1) bit))))))

;; Which gives our answer with the mini test input
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 mini-input)

;; And the test input
(part-1 test-input)

;; And the full input
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
(defn part-2
  [input]
  (println "Part 2"))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 input)
