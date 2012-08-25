(ns clojure_exercises.roman_nums
;  (:use '[clojure.contrib.math :as math])
  (:use clojure.test))

(def nums ["I" "V" "X" "L" "C" "D" "M"])
(defn roman-nums
  ([x]
    (if (>= x 4000)
        nil
        (roman-nums x 0 [])))
  ([x pos res]
    (if (zero? x)
        (apply str res)
        (let [y (rem x 10)
              rn-range (drop pos nums)
              rn (first rn-range)
              rnnext (second rn-range)
              rn2next (first (nnext rn-range))
              rnum (cond (= 1 y) rn
                         (= 2 y) (str rn rn)
                         (= 3 y) (str rn rn rn)
                         (= 4 y) (str rn rnnext)
                         (= 5 y) rnnext
                         (= 6 y) (str rnnext rn)
                         (= 7 y) (str rnnext rn rn)
                         (= 8 y) (str rnnext rn rn rn)
                         (= 9 y) (str rn rn2next)
                         (= 0 y) nil)]
          (recur (quot x 10) (+ pos 2) (if rnum (cons rnum res) res))))))

(deftest test-single-digits
  (is (= "I" (roman-nums 1)))
  (is (= "II" (roman-nums 2)))
  (is (= "III" (roman-nums 3)))
  (is (= "IV" (roman-nums 4)))
  (is (= "V" (roman-nums 5)))
  (is (= "VI" (roman-nums 6)))
  (is (= "VII" (roman-nums 7)))
  (is (= "VIII" (roman-nums 8)))
  (is (= "IX" (roman-nums 9)))
  (is (= "X" (roman-nums 10))))

(deftest test-11
  (is (= "XI" (roman-nums 11))))

(deftest test-50
  (is (= "L" (roman-nums 50))))

(deftest test-87
  (is (= "LXXXVII" (roman-nums 87))))

(deftest test-944
  (is (= "CMXLIV" (roman-nums 944))))

(deftest test-1977
  (is (= "MCMLXXVII" (roman-nums 1977))))

(deftest test-3999
  (is (= "MMMCMXCIX" (roman-nums 3999))))

(deftest test-4000
  (is (= nil (roman-nums 4000))))

(run-tests)
