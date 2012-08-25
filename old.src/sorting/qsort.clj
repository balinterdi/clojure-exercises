(ns sorting.qsort
  (:use clojure.contrib.test-is))

(defn qsort [coll]
  (if (empty? coll) []
    (let [p (first (drop (quot (count coll) 2) coll))]
      (lazy-cat 
        (qsort (filter #(< % p) coll))
        (filter #(= % p) coll)
        (qsort (filter #(> % p) coll))
      )
    )
  )
)

(deftest test-qsort
  (is (= [] (qsort [])))
  (is (= [1 2 3] (qsort [3 1 2])))
  (is (= [1 2 3] (qsort [3 2 1])))
  (is (= [1 2 3] (qsort [2 1 3])))
  (is (= (range 10) (qsort (reverse (range 10)))))
  (is (= [-2 0 3 5 6 11 18] (qsort [3 18 -2 6 0 11 5])))
)

(time (take 5 (qsort (reverse (range 1000))))) ; 2.418 msecs
(time (take 5 (qsort (reverse (range (apply * (take 4 (cycle [10])))))))) ; 7 msecs
(time (take 5 (qsort (reverse (range (apply * (take 5 (cycle [10])))))))) ; 144 msecs
(time (take 5 (qsort (reverse (range (apply * (take 6 (cycle [10])))))))) ; 883 msecs

(run-tests)