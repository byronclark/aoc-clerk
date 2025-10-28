^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day24
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]
   [util :as u]))

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

(defn- nodes->value
  [simulator prefix]
  (let [result-nodes (reverse (sort (filter #(str/starts-with? % prefix) (keys simulator))))]
    (->> result-nodes
         (map (fn [node] ((get simulator node) simulator)))
         (map #(if @% 1 0))
         (reduce (fn [acc bit]
                   (bit-or (bit-shift-left acc 1) bit))))))

(defn simulate
  [circuit]
  (let [simulator (->simulator circuit)]
    {:x (nodes->value simulator "x")
     :y (nodes->value simulator "y")
     :z (nodes->value simulator "z")}))

(defn part-1
  [circuit]
  (:z (simulate circuit)))

;; Which gives our answer with the mini test input
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 mini-input)

;; And the test input
(part-1 test-input)

;; And the full input
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
;;
;; Well this is *much* more fun.
;;
;; We have different test input for this part of the problem.
;; *WARNING*: the test input is for a circuit that performs bit-and instead of addition.
(def part-two-small-input (parse-input "x00: 0
x01: 1
x02: 0
x03: 1
x04: 0
x05: 1
y00: 0
y01: 0
y02: 1
y03: 1
y04: 0
y05: 1

x00 AND y00 -> z05
x01 AND y01 -> z02
x02 AND y02 -> z01
x03 AND y03 -> z03
x04 AND y04 -> z04
x05 AND y05 -> z00"))

;; Time to start with the naive approach. Figure out which output bits are wrong
;; and see how many wires lead to those bits. The trick here is probably that we
;; don't really care about the inputs we have as they may not expose all swapped
;; connections. The simplest case is trying to add all 1s with all 0s to see
;; what comes out as a zero.
(defn- register-bit-count
  [circuit prefix]
  (count (filter #(str/starts-with? % prefix) (keys circuit))))

(defn- replace-input
  [circuit prefix value]
  (reduce (fn [circuit n]
            (assoc circuit (format "%s%02d" prefix n)
                   {:type :input
                    :value (= 1 (bit-and 0x01 (bit-shift-right value n)))}))
          circuit
          (range (register-bit-count circuit prefix))))

(defn- all-ones
  [bits]
  (reduce (fn [acc _]
            (bit-or 0x01 (bit-shift-left acc 1)))
          0
          (range bits)))

(defn- bit-at
  [x n]
  (bit-and 0x01 (bit-shift-right x n)))

(defn- incorrect-result-bits-for-inputs
  [circuit input-bit-count x y expected]
  (let [test-circuit (-> circuit
                         (replace-input "x" x)
                         (replace-input "y" y))
        result (:z (simulate test-circuit))]
    (filter (fn [n] (not= (bit-at expected n) (bit-at result n)))
            (range input-bit-count))))

;; We'll test a variety of values to try to determine outputs that could possibly be wrong.
;; - provided inputs
;; - x = all ones, y = all zeros
;; - x = all ones, y = single one in each position
(defn- incorrect-result-bits
  [expected-fn circuit]
  (let [input-bit-count (max (register-bit-count circuit "x")
                             (register-bit-count circuit "y"))
        {x-initial :x y-initial :y} (simulate circuit)
        x-input (all-ones input-bit-count)
        y-inputs (->> (range input-bit-count)
                      (map #(bit-shift-left 1 %))
                      (cons 0))
        test-cases (->> y-inputs
                        (map (fn [y] [x-input y (expected-fn x-input y)]))
                        (cons [x-initial y-initial (expected-fn x-initial y-initial)]))]
    (reduce (fn [incorrect [x y expected]]
              (set/union incorrect
                         (set (incorrect-result-bits-for-inputs circuit input-bit-count x y expected))))
            #{}
            test-cases)))

;; Once we know the incorrect positions, we should see how many wires lead to
;; each of those positions to determine if we can reasonably brute force this or
;; not.
;;
;; Beware, this is making the assumption that the gates that need to be swapped
;; feed at least one of the wrong outputs.
(defn- nodes-to-output
  [circuit output-node]
  (letfn [(inner [visited current]
            (let [{node-type :type :as node-info} (get circuit current)
                  next-visited (conj visited current)]
              (if (= :input node-type)
                next-visited
                (->> (:inputs node-info)
                     (map #(inner next-visited %))
                     (apply set/union)))))]
    (inner #{} output-node)))

;; It turns out this doesn't help much. It trims 10 possible nodes from the full
;; problem set and 0 possible nodes from the test input.

;; It almost sounds like we need to brute force this. Which is terrifying.
;; Rightfully terrify because it's 2145 combinations to try *just* for the
;; smaller test input.
;;
;; But we can trim quite a bit of the space. Pairs that feed the same gate
;; *don't* matter because switching them will do nothing. Except not really
;; because a wire can be connected as an input on many gates.
;;
;; We should see how bad that is...
(defn- destinations-per-wire
  [circuit]
  (reduce (fn [wires node-info]
            (if (= :input (:type node-info))
              wires
              (reduce (fn [wires input]
                        (update wires input (fnil inc 0)))
                      wires
                      (:inputs node-info))))
          {}
          (vals circuit)))

{:nextjournal.clerk/visibility {:code :show :result :show}}
(->> part-two-small-input
     destinations-per-wire
     (sort-by second)
     reverse
     (take 10))

(->> input
     destinations-per-wire
     (sort-by second)
     reverse
     (take 10))

;; So the full input has *quite a few* nodes that serve as input for two gates.
;; But maybe we can still skip enough to get away with this.

;; *HOLD ON*, the full input is trying to build a full adder. This is probably a
;; case where it may be easier to visualize it and then look for the broken
;; connections.
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn ->dot
  [circuit]
  (let [input-bit-count (max (register-bit-count circuit "x")
                             (register-bit-count circuit "y"))
        input-nodes (mapcat (fn [n] [(format "x%02d" n)
                                     (format "y%02d" n)])
                            (reverse (range input-bit-count)))

        [nodes wires]
        (reduce (fn [[nodes wires] [node node-info]]
                  (let [inputs (:inputs node-info)
                        color (case (:operation node-info)
                                :xor "red"
                                :and "green"
                                :or "yellow")]
                    [(conj nodes (format "%s [style=filled,fillcolor=%s]" node color))
                     (concat wires
                               [(str (first inputs) " -> " node)
                                (str (second inputs) " -> " node)])]))
                [[] []]
                (reverse (sort (remove #(let [[node _] %]
                                          (or (str/starts-with? node "x")
                                              (str/starts-with? node "y")))
                                       circuit))))]
    (str/join "\n" (concat ["digraph C {"]
                           input-nodes
                           nodes
                           [""]
                           wires
                           ["}"]))))

(defn part-2
  [circuit filename]
  (spit filename (->dot circuit)))

;; Write the full input to a dot file so we can investigate.
(part-2 input "resources/2024-day24-part2-full.dot")

{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/caption "Original Circuit"
               (clerk/image "resources/2024-day24-part2-full.png"))

;; From visual inspection and review, we find:
{:nextjournal.clerk/visibility {:code :show :result :show}}
(def swapped #{"z06" "ksv"
               "nbd" "kbs"
               "tqq" "z20"
               "z39" "ckb"})

(->> swapped
     sort
     (str/join ","))

{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/caption "With misplaced wires swapped"
               (clerk/image "resources/2024-day24-part2-full-repaired.png"))
