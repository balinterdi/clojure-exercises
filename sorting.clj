(ns sorting
  (:use clojure.contrib.test-is))

(defn swap-even [coll]
  (let [swapped (apply concat (map (fn [[x y]] (if (> x y) [y x] [x y])) (partition 2 coll)))]
    (if (odd? (count coll))
      (lazy-cat swapped (take 1 (drop (dec (count coll)) coll)))
      swapped
    )
  )
)

(defn swap-odd [coll]
  (let [swapped (apply concat (map (fn [[x y]] (if (> x y) [y x] [x y])) (partition 2 (rest coll))))]
    (if (even? (count coll))
      (lazy-cat (take 1 coll) swapped (take 1 (drop (dec (count coll)) coll)))
      (lazy-cat (take 1 coll) swapped)
    )
  )
)

(defn bubble-sort [coll]
  "A bubble sort using lazy sequences"
  (if (empty? coll)
    []
    (let [sorter (fn [[coll n]]
          (let [swapper (if (even? n) swap-even swap-odd)] [(swapper coll) (inc n)]))]
        (first (nth (iterate sorter [coll 0]) (count coll)))
    )
  )
)

(deftest test-swap-even
  (is (= [] (swap-even [])))
  (is (= [1] (swap-even [1])))
  (is (= [1 2] (swap-even [1 2])))
  (is (= [1 2] (swap-even [2 1])))
  (is (= [3 4 1 2] (swap-even [4 3 2 1])))
  (is (= [4 5 2 3 1] (swap-even [5 4 3 2 1])))
)

(deftest test-swap-odd
  (is (= [] (swap-odd [])))
  (is (= [1] (swap-odd [1])))
  (is (= [1 2] (swap-odd [1 2])))
  (is (= [1 2] (swap-odd [1 2])))
  (is (= [3 1 2] (swap-odd [3 2 1])))
  (is (= [4 2 3 1] (swap-odd [4 3 2 1])))
  (is (= [5 3 4 1 2] (swap-odd [5 4 3 2 1])))
)

(deftest test-bubble-sort
  (is (= [] (bubble-sort [])))
  (is (= [1 2 3] (bubble-sort [3 1 2])))
  (is (= [1 2 3] (bubble-sort [3 2 1])))
  (is (= [1 2 3] (bubble-sort [2 1 3])))
  (is (= (range 10) (bubble-sort (reverse (range 10)))))
)

(run-tests)

; with non-lazy-sequences:
;(time (take 5 (bubble-sort-no-tco (reverse (range 1000))))) ; 23 seconds
;(time (take 5 (bubble-sort-tco (reverse (range 1000))))) ; 22 seconds
; with lazy-sequences:
;(time (take 5 (bubble-sort (reverse (range 1000))))) ; 3.69 seconds !!!
