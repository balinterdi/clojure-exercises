(ns helpers.math
  (:use clojure.contrib.test-is))

(defn pow [a n]
  ; nth operates in O(n) time for sequences?
  (last (take (inc n) (iterate (fn [x] (* x a)) 1)))
)
;  (take 4 (iterate (fn [x] [(last x) (* (last x) 10)]) [0 1]))

(deftest test-pow
  (is (= 1 (pow 2 0)))
  (is (= 16 (pow 2 4)))
)

(run-tests)
