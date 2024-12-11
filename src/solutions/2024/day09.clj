^{:nextjournal.clerk/visibility :hide-ns}
(ns solutions.2024.day09
  {:nextjournal.clerk/toc true}
  (:require
   [clojure.java.io :as io]
   [util :as u]
   [nextjournal.clerk :as clerk]
   [better-cond.core :as b]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "09" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :hide}}

;; # Solution
;;
;; Load and parse our input
;;
;; Part 1 only required parsing into the block by block layout, but Part 2 requires
;; keeping track of files and empty spaces for longer, so we'll parse into that form.
(defn parse [disk-map]
  (->> disk-map
       (map #(Character/digit % 10))
       (partition-all 2)
       (map-indexed vector)
       (mapcat (fn [[file-id [used free]]]
                 [{:kind :file
                   :file-id file-id
                   :blocks used}
                  {:kind :space
                   :blocks free}]))

       (filter #(when-let [blocks (:blocks %)]
                  (pos? blocks)))))



{:nextjournal.clerk/visibility {:code :show :result :show}}
(def input (->> (io/resource "inputs/2024/day09.txt")
                slurp
                parse))

(def test-input (parse "2333133121414131402"))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 1
;;
;; This part is easier to compute starting with a materialized disk so let's translate to that format.
(defn ->disk [block-map]
  (->> block-map
       (mapcat (fn [{:keys [kind blocks] :as chunk}]
                 (case kind
                   :space (repeat blocks nil)
                   :file (repeat blocks (:file-id chunk)))))
       vec))

;; Instead of materializing the entire compacted disk, we'll walk forward computing the
;; checksum filling blocks from the end as needed.
(defn part-1 [block-map]
  (let [disk (->disk block-map)]
    (loop [checksum 0
           left 0
           right (dec (count disk))]
      (b/cond
        (> left right)
        checksum

        :let [left-block (disk left)]

        (some? left-block)
        (recur (+ checksum (* left-block left)) (inc left) right)

        :let [right-block (disk right)]

        (some? right-block)
        (recur (+ checksum (* right-block left)) (inc left) (dec right))

        :else
        (recur checksum left (dec right))))))

;; Which gives our test input answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 test-input)

;; And our full input answer
(part-1 input)

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; ## Part 2
;;
;; This time we'll iterate over the block map and update it as we go.
;;
;; But it's not as obvious as I hoped. For every file on the right we have to find the
;; first space to the left of it that can hold the file. Only if there are no spaces do we
;; move to the next chunk.
;;
(defn first-space
  "Find the index of the first space in block-map that can accommodate a file of size blocks.

  Returns nil if no such space exists. "
  [block-map file-blocks]
  (->> block-map
       (map-indexed vector)
       (some (fn [[i {:keys [kind blocks]}]]
               (when (and (= :space kind)
                          (>= blocks file-blocks))
                 i)))))

;; Technically we should have to coalesce all the spaces after each move, but apparently
;; the puzzle input is designed so that it isn't required.
(defn compact-by-file [block-map]
  (loop [block-map (vec block-map)
         right (dec (count block-map))]
    (b/cond
      (zero? right)
      block-map

      :let [right-chunk (block-map right)]

      (= :space (:kind right-chunk))
      (recur block-map (dec right))

      :let [right-blocks (:blocks right-chunk)
            to-fill (first-space block-map right-blocks)]

      (and to-fill (< to-fill right))
      (let [left-chunk (block-map to-fill)
            left-blocks (:blocks left-chunk)
            replaced-left (keep identity [{:kind :file
                                           :file-id (:file-id right-chunk)
                                           :blocks right-blocks}
                                          (when (> left-blocks right-blocks)
                                            {:kind :space
                                             :blocks (- left-blocks right-blocks)})])
            replaced-right [{:kind :space
                             :blocks right-blocks}]
            compacted (vec (concat (subvec block-map 0 to-fill)
                                   replaced-left
                                   (subvec block-map (inc to-fill) right)
                                   replaced-right
                                   (when (< right (count block-map))
                                     (subvec block-map (inc right)))))]
        (recur compacted right))

      :else
      (recur block-map (dec right)))))

;; Which means we'll need a separate checksum function
(defn checksum [disk]
  (reduce (fn [acc [i block]]
            (if block
              (+ acc (* i block))
              acc))
          0
          (map-indexed vector disk)))

(defn part-2 [block-map]
  (->> block-map
       compact-by-file
       ->disk
       checksum))

;; Which gives our test input answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 test-input)

;; And our full input answer
(part-2 input)
