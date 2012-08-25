;; The Parallel Prefix Problem, applied to addition on integers
;; http://labs.oracle.com/projects/plrg/Publications/ICFPAugust2009Steele.pdf
;;
(ns clojure_exercises.parallel_prefix
  (:require [clojure.test :as test]))

; This is the serial (sequential) version
(defn sprefix
  ([coll]
    (sprefix coll [] 0))
  ([coll running-totals sum]
    (if (empty? coll)
      [running-totals sum]
      (recur (rest coll) (conj running-totals sum) (+ (first coll) sum)))))

;;
;; See Slide #50 from
;; http://labs.oracle.com/projects/plrg/Publications/ICFPAugust2009Steele.pdf
(defn pprefix
  ([coll]
   (pprefix 0 coll))
  ([s coll]
   (cond
    (empty? coll) (vector [] s)
    (= (count coll) 1) (vector [s] (+ s (first coll)))
    :else
     (let [split (quot (count coll) 2)
           coll1 (subvec coll 0 split)
           coll2 (subvec coll split (count coll))
           [coll1-rts s1] (pprefix s coll1) ;rts = running totals
           [coll2-rts s2] (pprefix s1 coll2)]
       (vector (into coll1-rts coll2-rts) s2)))))
;;
;;TODO: Can the above use the fold function in the reducers library
;; see http://clojure.com/blog/2012/05/08/reducers-a-library-and-model-for-collection-processing.html

; Tests
(test/deftest test-empty-coll
  (test/is
    (= (sprefix []) [[], 0])))

(test/deftest test-one-item-coll
  (test/is
    (= (sprefix [2]) [[0], 2])))

(test/deftest test-longer-colls
  (test/is
    (= (sprefix [1 2 3 6 3 4 -5 2 9]) [[0 1 3 6 12 15 19 14 16], 25])))

(test/deftest test-empty-coll-parallel
  (test/is
    (= (pprefix []) [[], 0])))

(test/deftest test-one-item-coll-parallel
  (test/is
    (= (pprefix [2]) [[0], 2])))

(test/deftest test-longer-colls-parallel
  (test/is
    (= (pprefix [1 2 3 6 3 4 -5 2 9]) [[0 1 3 6 12 15 19 14 16], 25])))


(test/run-tests)

