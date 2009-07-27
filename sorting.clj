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
      (swap-all (conj (drop 2 elts) b) func (conj swapped a))
    )
  )
)

(defn swap-all
  ([elts func]
    (swap-all elts func []))
  ([elts func swapped]
    (if (< (count elts) 2)
      (concat swapped elts)
      (let [[a b] (swap-if (take 2 elts) func)]
        ;(println (str "A: " a " B: " b " Elts: " elts " Swapped: " swapped))
        (swap-all (conj (drop 2 elts) b) func (conj swapped a))
      )
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

(defn bubble-sort
  "A bubble sort with tail call optimization (TCO)"
  ([elts]
    (bubble-sort elts []))
  ([elts sorted]
    (if (empty? elts)
      sorted
      (let [swapped (swap-all elts > [])
            done (last swapped)
            others (butlast swapped)]
        ;(println (str "Done: " done " others: " others " Elts: " elts " Sorted: " sorted))
        (recur others (lazy-seq (cons done sorted)))
      )
    )
  )
)



(deftest test-swap-if
  (is (= [] (swap-if [] >)))
  (is (= [1] (swap-if [1] >)))
  (is (= [1 2] (swap-if [1 2] >)))
  (is (= [1 2] (swap-if [2 1] >)))
)

(deftest test-swap-all
  (is (= [] (swap-all [] > [])))
  (is (= [1] (swap-all [1] > [])))
  (is (= [1 2] (swap-all [1 2] > [])))
  (is (= [1 2] (swap-all [2 1] > [])))
  (is (= [2 1 3] (swap-all [3 2 1] > [])))
  (is (= [1 2 3] (swap-all [2 1 3] > [])))
  (is (= [1 2 3 4] (swap-all [2 1 4 3] > [])))
)

;(deftest test-bubble-sort
;  (is (= [] (bubble-sort [])))
;  (is (= [1 2 3] (bubble-sort [3 1 2])))
;  (is (= [1 2 3] (bubble-sort [3 2 1])))
;  (is (= [1 2 3] (bubble-sort [2 1 3])))
;  (is (= (range 10) (bubble-sort (reverse (range 10)))))
;)

(run-tests)

;(time (take 5 (bubble-sort-no-tco (reverse (range 1000))))) ; 23 seconds
;(time (take 5 (bubble-sort-tco (reverse (range 1000))))) ; 22 seconds
;(time (take 5 (bubble-sort (reverse (range 1000)))))