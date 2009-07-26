(ns sorting
  (:use clojure.contrib.test-is))

(defn swap-if [elts func]  
  (let [a (first elts) 
        b (last elts)]
    (cond (< (count elts) 2) elts
      (func a b) (list b a)
      true (list a b))
    )
)

; swaps all pairs if func is true for the first of the pair
(defn swap-all [elts func swapped i]
  (if (or (< (count elts) 2) (= i 10))
    (concat swapped elts)
    (let [[a b] (swap-if (take 2 elts) func)]
      ;(println (str "A: " a " B: " b " Elts: " elts " Swapped: " swapped))
      (swap-all (conj (drop 2 elts) b) func (conj swapped a) (inc i))
    )
  )
)

; the last one in elts is always sorted already so
; 1. swap-all is executed on elts
; 2. the last element is consed with sorted
; 3. bubble-sort is called on the elts minus the last element
(defn bubble-sort [elts sorted]
  (if (empty? elts)
    sorted
    (let [swapped (swap-all elts > [] 0) 
          done (last swapped)
          others (butlast swapped)]
      ;(println (str "Done: " done " others: " others " Elts: " elts " Sorted: " sorted))
      (bubble-sort others (cons done sorted))
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
  (is (= [] (swap-all [] > [] 0)))
  (is (= [1] (swap-all [1] > [] 0)))
  (is (= [1 2] (swap-all [1 2] > [] 0)))
  (is (= [1 2] (swap-all [2 1] > [] 0)))
  (is (= [2 1 3] (swap-all [3 2 1] > [] 0)))
  (is (= [1 2 3] (swap-all [2 1 3] > [] 0)))
  (is (= [1 2 3 4] (swap-all [2 1 4 3] > [] 0)))
)

(deftest test-bubble-sort
  (is (= [] (bubble-sort [] [])))
  (is (= [1 2 3] (bubble-sort [3 1 2] [])))
  (is (= [1 2 3] (bubble-sort [3 2 1] [])))
  (is (= [1 2 3] (bubble-sort [2 1 3] [])))
)

(run-tests)
