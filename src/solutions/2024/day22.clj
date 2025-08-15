^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day22
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]
   [util :as u]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "22" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; Load and parse our input
(def input (->> (io/resource "inputs/2024/day22.txt")
                slurp
                str/split-lines
                (map parse-long)))

;; And the smaller test input
(def test-input [1 10 100 2024])

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; We'll start with the naive version of generating the secret number. And by
;; naive I mean we can use bitwise operations, but we'll do each operation
;; separately. Once it's working, I bet we'll need a simpler version of the
;; operations.
;;
;; Here's the sequence to get the next number:
;; ### Step 1
;; 1. multiply by 64 ($n_p \ll 6$)
;; 2. mix: XOR with previous value ($n \oplus n_p$)
;; 3. prune: modulo 16777216 ($n \land \text{0xFFFFFF}$)
(defn- step-one
  [n]
  (-> n
      (bit-shift-left 6)
      (bit-xor n)
      (bit-and 0x0ffffff)))

;; ### Step 2
;; 1. divide by 32 ($n \gg 5$)
;; 2. mix
;; 3. prune
(defn- step-two
  [n]
  (-> n
      (bit-shift-right 5)
      (bit-xor n)
      (bit-and 0x0ffffff)))

;; ### Step 3
;; 1. multiply by 2048 ($n \ll 11$)
;; 2. mix
;; 3. prune
(defn- step-three
  [n]
  (-> n
      (bit-shift-left 11)
      (bit-xor n)
      (bit-and 0x0ffffff)))

(defn next-n
  [n]
  (-> n
      step-one
      step-two
      step-three))

;; Since we can, why not generate an infinite sequence of secret numbers and
;; only materialize the ones we need.
(defn secret-numbers [n]
  (lazy-seq (cons n (secret-numbers (next-n n)))))

;; This is going to be slow and might only work for the test input, but we'll
;; try.
(defn nth-secret [n initial]
  (->> (secret-numbers initial)
       (drop n)
       first))

;; So we'll take the 2000th iteration of each input and then add them up
(defn part-1
  [initials]
  (->> initials
       (map (partial nth-secret 2000))
       (apply +)))

;; Which gives our answer with the test input:
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 test-input)

;; And the full input:
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
;;
;; We've got an additional test case for this one:
(def part-2-test-input [1 2 3 2024])

;; And we need to calculate all the four number sequences and the number of
;; bananas they first lead to.

;; It took me an embarrassingly long time to realize that $n % 10$ is the
;; easiest way to get the last digit of the number. And takes significantly less
;; time than the string conversion and grabbing the last character.
(defn last-digit
  "Extract the last decimal digit of n."
  [n]
  (mod n 10))

(defn bananas
  [initial]
  (->> initial
       secret-numbers
       (map last-digit)))

(defn with-changes
  [n initial]
  (let [bananas-seq (take n (bananas initial))
        changes (->> bananas-seq
                     (partition 2 1)
                     (map (fn [[a b]] (- b a))))]
    (map vector bananas-seq (cons nil changes))))

;; Now finally for the first sequence that leads to each number of bananas (if
;; we have one). But that's not all we need here. We also need to know where
;; each sequence leads.
(defn sequences-for-bananas
  [n initial]
  (->> (with-changes n initial)
       rest ;; skip the first as it's not a change
       (partition 4 1)
       (reduce (fn [{:keys [only-first targets]} segment]
                 (let [n-bananas (first (last segment))
                       sequence (map second segment)
                       targets (if (contains? targets sequence)
                                 targets
                                 (assoc targets sequence n-bananas))
                       only-first (if (contains? only-first n-bananas)
                                    only-first
                                    (assoc only-first n-bananas sequence))]
                   {:only-first only-first
                    :targets targets}))
               {})))

;; Those methods worked perfectly, with one slight problem. Calculating the
;; changes for all of these takes way too long. It's ~5s per 200 initial numbers
;; so we're looking at ~50s for all of them.
;;
;; While I did successfully solve it, I'm not very happy with taking almost a
;; minute for this to run. There's got to be a better way.
;;
;; Let's take a little more time to look at these numbers.
;;
;; Surprise #1: There doesn't seem to be any periodicity to the last decimal
;; digit of the secret numbers.
;;
;; What we really need to know is how to calculate the change in the last
;; decimal digit from the last value.
;;
;; Leaving the last nine bits of the number as 0 always results in the 4 least
;; significant bits being 0. Not sure that helps much.
(->> (range 20)
     (map #(bit-shift-left % 9))
     (map #(vec [(format "%08x" %) (format "%08x" (next-n %))])))

;; We know from the prune operation that no subsequent number can depend on more
;; than the 24 least-significant bits of the previous number.
(defn secret-numbers-truncated [n]
  (let [byte-n (bit-and 0xffffff n)]
    (lazy-seq (cons byte-n (secret-numbers-truncated (next-n byte-n))))))

;; Let's see how it compares:
(map (fn [& args] (mapv #(format "%02x" (bit-and 0xff %)) args))
     (take 10 (secret-numbers 123))
     (take 10 (secret-numbers-truncated 123)))

;; Performance wise, it didn't really help. Even though the range was now small
;; enough to memoize.

;; And really, the biggest win was making my last digit function faster. That
;; takes us down to ~20 seconds for the full solution. At this point I think
;; that's good enough.

{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [initials]
  (let [sequence-maps (mapv (partial sequences-for-bananas 2000) initials)
        all-sequences (->> sequence-maps
                           (map :only-first)
                           (mapcat vals)
                           set)]
    (->> all-sequences
         (map (fn [sequence]
                  (->> sequence-maps
                       (keep #(get-in % [:targets sequence]))
                       (apply +))))
         (apply max))))

;; Which gives our answer for the part 2 test case
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 part-2-test-input)

;; And the full input
(part-2 input)
