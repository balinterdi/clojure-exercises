(ns helpers.seqs
  (:use helpers.math)
  (:use clojure.contrib.test-is))

; TODO: Find a better algorythm or look up how the Clojure guys do it.
; Stack overflow
; (time (my-partition 10 (range 1 (pow 10 5))) )
(defn my-partition [n coll]
 (if (empty? coll)
  coll
  (cons (take n coll) (my-partition n (drop n coll)))))

(deftest test-partition
  (is (= (my-partition 20 (range 1 101)) (partition 20 (range 1 101)) ))
)

(run-tests)
