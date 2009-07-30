(ns sorting
  (:use clojure.contrib.test-is))

(defn swap-in-coll [coll index func]
  (cond
    (nil? (nth coll (inc index) nil)) coll
    (func (nth coll index) (nth coll (inc index)))
      (let [lead (take index coll)
            swapped-ahead (take 1 (drop (inc index) coll))
            swapped-behind (take 1 (drop index coll))
            rest (drop (+ 2 index) coll)]
            (concat lead swapped-ahead swapped-behind rest)
      )
    :else coll
  )
)

(defn swap-all
  ([coll func]
    (swap-all coll 0 func))
  ([coll index func]
    (if (= index (count coll)) coll
      (recur (swap-in-coll coll index func) (inc index) func)))
)

(defn bubble-sort [elts]
  "A bubble sort using lazy sequences"
  (if (empty? elts)
    []
    (nth (iterate (fn [e] (swap-all e >)) elts) (dec (count elts))))
)

(deftest test-swap-all
  (is (= [] (swap-all [] >)))
  (is (= [1] (swap-all [1] >)))
  (is (= [1 2] (swap-all [1 2] >)))
  (is (= [1 2] (swap-all [2 1] >)))
  (is (= [2 1 3] (swap-all [3 2 1] >)))
  (is (= [1 2 3] (swap-all [2 1 3] >)))
  (is (= [1 2 3 4] (swap-all [2 1 4 3] >)))
)

(deftest test-swap-in-coll
  (is (= [] (swap-in-coll [] 0 >)))
  (is (= [] (swap-in-coll [] 2 >)))
  (is (= [1] (swap-in-coll [1] 0 >)))
  (is (= [1] (swap-in-coll [1] 2 >)))
  (is (= [1 2] (swap-in-coll [1 2] 0 >)))
  (is (= [1 2] (swap-in-coll [2 1] 0 >)))
  (is (= [2 1] (swap-in-coll [2 1] 1 >)))
  (is (= [4 2 3 1] (swap-in-coll [4 3 2 1] 1 >)))
  (is (= [4 3 1 2] (swap-in-coll [4 3 2 1] 2 >)))
  (is (= [3 4 2 1] (swap-in-coll [4 3 2 1] 0 >)))
)

(deftest test-bubble-sort
  (is (= [] (bubble-sort [])))
  (is (= [1 2 3] (bubble-sort [3 1 2])))
  (is (= [1 2 3] (bubble-sort [3 2 1])))
  (is (= [1 2 3] (bubble-sort [2 1 3])))
  (is (= (range 10) (bubble-sort (reverse (range 10)))))
)

(run-tests)

;(time (take 5 (bubble-sort-no-tco (reverse (range 1000))))) ; 23 seconds
;(time (take 5 (bubble-sort-tco (reverse (range 1000))))) ; 22 seconds
;(time (take 5 (bubble-sort (reverse (range 1000))))) ; 769 seconds :(
