(ns solutions.2024.helpers.pathfinding
  (:require
   [better-cond.core :as bc]
   [clojure.set :as set]))

(defn a*
  "A* without a heuristic... so it's Dijkstra's algorithm in a shiny coat.

  Takes a variety of functions to remain generic:
  - connections-fn - given a node, returns all possible connections
  - target-fn - given a connection, returns the target node
  - cost-fn - given a connection, returns the cost to follow the connection
  - done?-fn - is the given node the end?"
  [start-node {:keys [connections-fn target-fn cost-fn done?-fn]}]
  (let [MAX-VALUE Long/MAX_VALUE]
    (loop [open-set #{start-node}
           g-scores {start-node 0}]
      (bc/cond
        (empty? open-set) ;no path available
        nil

        :let [current (apply min-key #(get g-scores % MAX-VALUE) open-set)
              current-g-score (get g-scores current)]
        (done?-fn current)
        current-g-score

        :let [[open-set' g-scores']
              (reduce (fn [[open-set g-scores] connection]
                        (let [target (target-fn connection)
                              test-g-score (+ current-g-score (cost-fn connection))]
                          (if (< test-g-score (get g-scores target MAX-VALUE))
                            [(conj open-set target) (assoc g-scores target test-g-score)]
                            [open-set g-scores])))
                      [(disj open-set current) g-scores]
                      (connections-fn current))]
        :else
        (recur open-set' g-scores')))))

;; We'll start with A* but we need to store the path that got to each $g(n)$. Unlike our
;; original implementation we'll need to handle equal cost differently. Instead of
;; replacing the paths to that point, we'll collect the new paths and keep the existing
;; ones.
(defn a*-with-path
  "A* without a heuristic that saves the paths followed... so it's Dijkstra's algorithm and stores the path.

  Takes a variety of functions to remain generic:
  - connections-fn - given a node, returns all possible connections
  - target-fn - given a connection, returns the target node
  - cost-fn - given a connection, returns the cost to follow the connection
  - done?-fn - is the given node the end?"
  [start-node {:keys [connections-fn target-fn cost-fn done?-fn]}]
  (let [MAX-VALUE Long/MAX_VALUE]
    (loop [open-set #{start-node}
           paths {start-node #{[start-node]}}
           g-scores {start-node 0}]
      (bc/cond
        (empty? open-set) ;no path available
        nil

        :let [current (apply min-key #(get g-scores % MAX-VALUE) open-set)
              current-g-score (get g-scores current)
              current-paths (get paths current)]
        (done?-fn current)
        current-paths

        :let [[open-set' paths' g-scores']
              (reduce (fn [[open-set paths g-scores] connection]
                        (let [target (target-fn connection)
                              target-g-score (get g-scores target)
                              test-g-score (+ current-g-score (cost-fn connection))
                              target-paths (set (map #(conj % target) current-paths))]
                          (cond
                            ;; Better path than we've found so far
                            (or (nil? target-g-score)
                                (< test-g-score target-g-score))
                            [(conj open-set target)
                             (assoc paths target target-paths)
                             (assoc g-scores target test-g-score)]

                            ;; Equivalent path
                            (= test-g-score target-g-score)
                            [(conj open-set target)
                             (update paths target (fnil set/union #{}) target-paths)
                             (assoc g-scores target test-g-score)]

                            :else
                            [open-set paths g-scores])))
                      [(disj open-set current) paths g-scores]
                      (connections-fn current))]
        :else
        (recur open-set' paths' g-scores')))))
