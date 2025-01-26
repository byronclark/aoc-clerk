^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day17
  {:nextjournal.clerk/toc true}
  (:require
   [better-cond.core :as bc]
   [clojure.java.io :as io]
   [clojure.math :as math]
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]
   [util :as u]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "17" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :hide}}

;; # Solution
;;
;; Load and parse our input by building the state of the computer ready to run.
(defn load-computer
  [input]
  (let [[register-lines program-line] (str/split input #"\n\n")]
    {:instruction-pointer 0
     :registers (->> register-lines
                     str/split-lines
                     (map #(-> % (str/split #": ") last parse-long))
                     (map vector [:a :b :c])
                     (into {}))
     :program (-> program-line
                  (str/split #": " 2)
                  last
                  str/trim
                  (str/split #",")
                  ((partial mapv parse-long)))
     :outputs []}))

{:nextjournal.clerk/visibility {:code :show :result :show}}
(def input (-> (io/resource "inputs/2024/day17.txt")
               slurp
               load-computer))

;; And a smaller test input
(def test-input (load-computer "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"))


{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; Time to implement an emulator for this processor
;;
;; We'll need an easy way to evaluate combo operands
(defn- combo-value
  [computer operand]
  (case operand
    (0 1 2 3) operand
    4 (-> computer :registers :a)
    5 (-> computer :registers :b)
    6 (-> computer :registers :c)))

;; And handling for each opcode
(defn- next-instruction
  "Common case: advance instruction pointer to the next instruction."
  [computer]
  (update computer :instruction-pointer + 2))

(defn- op-adv
  [{:keys [registers] :as computer} operand]
  (let [value (long (/ (:a registers)
                       (math/pow 2 (combo-value computer operand))))]
    (-> computer
        (assoc-in [:registers :a] value)
        next-instruction)))

(defn- op-bxl
  [{:keys [registers] :as computer} operand]
  (let [value (bit-xor (:b registers) operand)]
    (-> computer
        (assoc-in [:registers :b] value)
        next-instruction)))

(defn- op-bst
  [computer operand]
  (let [value (mod (combo-value computer operand) 8)]
    (-> computer
        (assoc-in [:registers :b] value)
        next-instruction)))

(defn- op-jnz
  [{:keys [registers] :as computer} operand]
  (if-not (zero? (:a registers))
    (assoc computer :instruction-pointer operand)
    (next-instruction computer)))

(defn- op-bxc
  [{:keys [registers] :as computer} _operand]
  (let [value (bit-xor (:b registers) (:c registers))]
    (-> computer
        (assoc-in [:registers :b] value)
        next-instruction)))

(defn- op-out
  [computer operand]
  (let [value (mod (combo-value computer operand) 8)]
    (-> computer
        (update :outputs conj value)
        next-instruction)))

(defn- op-bdv
  [{:keys [registers] :as computer} operand]
  (let [value (long (/ (:a registers)
                       (math/pow 2 (combo-value computer operand))))]
    (-> computer
        (assoc-in [:registers :b] value)
        next-instruction)))

(defn- op-cdv
  [{:keys [registers] :as computer} operand]
  (let [value (long (/ (:a registers)
                       (math/pow 2 (combo-value computer operand))))]
    (-> computer
        (assoc-in [:registers :c] value)
        next-instruction)))

;; This is the core of the emulator. We fetch the next instruction, dispatch it, the
;; return the next state of the computer.
(defn- step
  [{:keys [instruction-pointer program] :as computer}]
  (if (>= instruction-pointer (count program))
    (assoc computer :instruction-pointer :halt)
    (let [instruction (nth program instruction-pointer)
          operand (nth program (inc instruction-pointer))]
      ((case instruction
         0 op-adv
         1 op-bxl
         2 op-bst
         3 op-jnz
         4 op-bxc
         5 op-out
         6 op-bdv
         7 op-cdv) computer operand))))

(defn- run-computer
  [computer]
  (loop [computer computer]
    (if (= :halt (:instruction-pointer computer))
      (:outputs computer)
      (recur (step computer)))))

(defn part-1
  [computer]
  (str/join "," (run-computer computer)))

;; Which gives our answer for the test input
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 test-input)

;; And the full input
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
;;
;; I'm really hoping this works with counting up until we have a winner. Let's try to fail
;; fast and see how far we get.
(defn- outputs-self?
  [computer a]
  (loop [computer (assoc-in computer [:registers :a] a)
         remaining (:program computer)]
    (bc/cond
      (empty? remaining)
      true

      (= :halt (:instruction-pointer computer))
      false

      :let [computer' (step computer)
            outputs' (:outputs computer')
            new-output? (not= (count outputs') (count (:outputs computer)))]

      (and new-output? (not= (last outputs') (first remaining)))
      false

      :else
      (recur computer' (if new-output? (rest remaining) remaining)))))

;; And that's fast enough for the special test input but nowhere near fast enough for the
;; full input. Or even the test input from part 1. We'll need something else.

;; We have a special test input for part 2; let's take a closer look.
{:nextjournal.clerk/visibility {:code :show :result :show}}
(def quine-input (load-computer "Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0"))

;; As written we get these outputs:
(part-1 quine-input)

;; While we have get the original input when we use 117440 as the value stored in register a:
(part-1 (assoc-in quine-input [:registers :a] 117440))

;; Let's take a better look at that input as octal digits:
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn ->printable-digits [n]
  (format "%o" n))

{:nextjournal.clerk/visibility {:code :show :result :show}}
;; Original:
(->printable-digits 2024)
(part-1 quine-input)

;; Quine:
(->printable-digits 117440)
(part-1 (assoc-in quine-input [:registers :a] 117440))

;; It looks like what's happening with this program is that the least significant octal digit is being removed and then each least significant digit is output.

;; Let's look at the full program
(get-in input [:registers :a])
(->printable-digits (get-in input [:registers :a]))
(part-1 input)

;; Definitely some translation happening here so let's look at the program:
;; ```
;; 2,4 - bst a - place least significant digit of a in b
;; 1,2 - bxl 2 - xor b 2
;; 7,5 - cdv b - a / 2^b -> c (aka shift right b)
;; 0,3 - adv 3 - a / 8 -> a (aka shift right 3)
;; 4,7 - bxc _ - xor b c
;; 1,7 - bxl 7 - xor b 7 (aka not)
;; 5,5 - out b - output b
;; 3,0 - jnz 0 - goto 0
;; ```
;;
;; Let's start with a = 0b110011 which should result in b = 0b111 before the output:
(part-1 (assoc-in input [:registers :a] 063))

;; ```
;; bst a -> b = 0b011
;; bxl 2 -> b = 0b001
;; cdv b -> c = 0b110011 / 2^1 = 0b011001
;; adv 3 -> a = 0b110011 / 2^3 = 0b000110
;; bxc _ -> b = 0b001 ^ 0b011001 = 0b011000
;; bxl 7 -> b = 0b011111
;; OUTPUT 0b111
;; Back to the beginning
;; bst a -> b = 0b110
;; bxl 2 -> b = 0b100
;; cdv b -> c = 0b000110 / 2^6 = 0b000000
;; adv 3 -> a = 0b000000
;; bxc _ -> b = 0b100 ^ 0b000000
;; bxl 7 -> b = 0b011
;; OUTPUT 0b011
;; ```

;; So for our program each output is:
;;
;; b = (0x7 & a) ^ 2
;;
;; c = (a >> b)
;;
;; a = (a >> 3)
;;
;; out -> (0x7 & !(b ^ c))
;;
;; | 0x7 & a | b | c      | out              |
;; |---------|---|--------|------------------|
;; | 0       | 2 | a >> 2 | !(0x2 ^ a >> 2)  |
;; | 1       | 3 | a >> 3 | !(0x3 ^ a >> 3)  |
;; | 2       | 0 | a      | !(0x0 ^ a)       |
;; | 3       | 1 | a >> 1 | !(0x1 ^ a >> 1)  |
;; | 4       | 6 | a >> 6 | !(0x6 ^ a >> 6)  |
;; | 5       | 7 | a >> 7 | !(0x7 ^ a >> 7)  |
;; | 6       | 4 | a >> 4 | !(0x4 ^ a >> 4)  |
;; | 7       | 5 | a >> 5 | !(0x5 ^ a >> 5)  |

;; So it looks like up to 10 bits can influence each output. Let's build a table of these:
(->> (range 00000 01777)
     (map #(vector (format "%04o" %) (part-1 (assoc-in input [:registers :a] %))))
     (group-by second))

;; Now that we know the number of octal digits in the output... can we brute force it?
;; That's only 8^16 solutions to check... We can trim it a little because we know the
;; first digit can't be 0. That's still a lot of values to check.
(- 07777777777777777 01000000000000000)

;; Doing this by hand looking at that table isn't going to go well either. Maybe it's time
;; to look at how to get different digits.

{:nextjournal.clerk/visibility {:code :show :result :hide}}
(def output-mapping
  "Lovingly generated by hand."
  (->> [{:out "101" :lsb "000" :prev "x00"}
        {:out "111" :lsb "000" :prev "x01"}
        {:out "001" :lsb "000" :prev "x10"}
        {:out "011" :lsb "000" :prev "x11"}
        {:out "100" :lsb "001" :prev "000"}
        {:out "101" :lsb "001" :prev "001"}
        {:out "110" :lsb "001" :prev "010"}
        {:out "111" :lsb "001" :prev "011"}
        {:out "000" :lsb "001" :prev "100"}
        {:out "001" :lsb "001" :prev "101"}
        {:out "010" :lsb "001" :prev "110"}
        {:out "011" :lsb "001" :prev "111"}
        {:out "101" :lsb "010" :prev "xxx"}
        {:out "111" :lsb "011" :prev "xx0"}
        {:out "011" :lsb "011" :prev "xx1"}
        {:out "001" :lsb "100" :prev "000xxx"}
        {:out "000" :lsb "100" :prev "001xxx"}
        {:out "011" :lsb "100" :prev "010xxx"}
        {:out "010" :lsb "100" :prev "011xxx"}
        {:out "101" :lsb "100" :prev "100xxx"}
        {:out "100" :lsb "100" :prev "101xxx"}
        {:out "111" :lsb "100" :prev "110xxx"}
        {:out "110" :lsb "100" :prev "111xxx"}
        {:out "110" :lsb "100" :prev "111xxx"}
        {:out "000" :lsb "101" :prev "000xxxx"}
        {:out "001" :lsb "101" :prev "001xxxx"}
        {:out "010" :lsb "101" :prev "010xxxx"}
        {:out "011" :lsb "101" :prev "011xxxx"}
        {:out "100" :lsb "101" :prev "100xxxx"}
        {:out "101" :lsb "101" :prev "101xxxx"}
        {:out "110" :lsb "101" :prev "110xxxx"}
        {:out "111" :lsb "101" :prev "111xxxx"}
        {:out "011" :lsb "110" :prev "000x"}
        {:out "010" :lsb "110" :prev "001x"}
        {:out "001" :lsb "110" :prev "010x"}
        {:out "000" :lsb "110" :prev "011x"}
        {:out "111" :lsb "110" :prev "100x"}
        {:out "110" :lsb "110" :prev "101x"}
        {:out "101" :lsb "110" :prev "110x"}
        {:out "110" :lsb "110" :prev "111x"}
        {:out "010" :lsb "111" :prev "000xx"}
        {:out "011" :lsb "111" :prev "001xx"}
        {:out "000" :lsb "111" :prev "010xx"}
        {:out "001" :lsb "111" :prev "011xx"}
        {:out "110" :lsb "111" :prev "100xx"}
        {:out "111" :lsb "111" :prev "101xx"}
        {:out "100" :lsb "111" :prev "110xx"}
        {:out "101" :lsb "111" :prev "111xx"}]
       (group-by :out)
       (sort-by first)))

{:nextjournal.clerk/visibility {:code :show :result :show}}
;; Expected result
(str/join "," (:program input))

;; With that built, I tried to figure out the input from the end to the beginning since I
;; had a map of which bits could be used before each digit. And, yes, I did it by
;; hand. No, it didn't work but mostly because I couldn't keep track of the
;; branches. That's what computers are for.

;; Let's try this the other way around. Start with an empty input and try all the possible
;; first digits to see which gives the right *last* octal digit of the program. Then, do a
;; depth first search adding each digit.

;; So. Embarrassingly. Easy.

;; Doesn't even require deep knowledge of how this works other than knowing that octal
;; digits matter.

{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn- check-a-from
  [computer from valid-digits]
  (let [program (:program computer)
        expected (take-last valid-digits (:program computer))
        output (run-computer (assoc-in computer [:registers :a] from))]
    (when (or (zero? valid-digits)
              (= output expected))
      (cond
        (= output program)
        from

        (> (count output) (count program))
        nil

        :else
        (->> (range 8)
             (map (fn [lsb] (bit-or (bit-shift-left from 3)
                                    lsb)))
             (some #(check-a-from computer % (inc valid-digits))))))))

(defn part-2
  [computer]
  (check-a-from computer 0 0))

;; Which gives us the result for the quine example:
{:nextjournal.clerk/visibility {:code :hide :result :show}}

(part-2 quine-input)

;; And the full input:
(part-2 input)
