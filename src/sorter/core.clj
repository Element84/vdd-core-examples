(ns sorter.core
  (:use [vdd-core.capture-global :only [capture!]]))

(defn qsort
  "An implementation of quicksort that captures intermediate data.
  Based on http://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Clojure"
  [[pivot & xs]]
  (when pivot
    (let [smaller #(< % pivot)
          before-pivot (filter smaller xs)
          after-pivot (remove smaller xs)]
      (capture! {:left before-pivot
                 :pivot pivot
                 :right after-pivot})
      (lazy-cat (qsort before-pivot)
                [pivot]
                (qsort after-pivot)))))

