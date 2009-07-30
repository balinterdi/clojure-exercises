(ns sorting
  (:use clojure.contrib.test-is))

(defn swap-if [elts func]
  (let [a (first elts)
        b (last elts)]
    (if (and a b (func a b)) (reverse elts) elts) ; reverse is not lazy!
  )
)

; swaps all pairs if func is true for the first of the pair
(defn swap-all-not-perfect [elts func swapped]
  (if (< (count elts) 2)
    (concat swapped elts)
    (let [[a b] (swap-if (take 2 elts) func)]
      ;(println (str "A: " a " B: " b " Elts: " elts " Swapped: " swapped))
      (swap-all-not-perfect (conj (drop 2 elts) b) func (conj swapped a))
    )
  )
)

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
    (do
      (println "Coll: " coll " Idx: " index)
      (if (= index (count coll)) coll
        (let [new_coll (swap-in-coll coll index func)]
          (do
            (println "New coll: " new_coll)
            (recur new_coll (inc index) func))))
    )
  )
)

; the last one in elts is always sorted already so
; 1. swap-all is executed on elts
; 2. the last element is consed with sorted
; 3. bubble-sort is called on the elts minus the last element
(defn bubble-sort-no-tco
  "A bubble sort without tail call optimization (TCO)"
  ([elts]
    (bubble-sort-no-tco elts []))
  ([elts sorted]
    (if (empty? elts)
      sorted
      (let [swapped (swap-all elts > [])
            done (last swapped)
            others (butlast swapped)]
        ;(println (str "Done: " done " others: " others " Elts: " elts " Sorted: " sorted))
        (bubble-sort-no-tco others (cons done sorted))
      )
    )
  )
)

(defn bubble-sort-tco
  "A bubble sort with tail call optimization (TCO)"
  ([elts]
    (bubble-sort-tco elts []))
  ([elts sorted]
    (if (empty? elts)
      sorted
      (let [swapped (swap-all elts > [])
            done (last swapped)
            others (butlast swapped)]
        ;(println (str "Done: " done " others: " others " Elts: " elts " Sorted: " sorted))
        (recur others (cons done sorted))
      )
    )
  )
)

(defn bubble-sort [elts]
  "A bubble sort using lazy sequences"
  (if (empty? elts)
    []
    (nth (iterate (fn [e] (swap-all e >)) elts) (dec (count elts))))
)

(deftest test-swap-if
  (is (= [] (swap-if [] >)))
  (is (= [1] (swap-if [1] >)))
  (is (= [1 2] (swap-if [1 2] >)))
  (is (= [1 2] (swap-if [2 1] >)))
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
;(time (take 5 (bubble-sort (reverse (range 1000)))))