; Find the 9-digit number which contains all the digits from 1 to 9,
; which is divisible by 9, if you remove the last digit becomes divisible by 8,
; and so on all the way.

; author: Balint Erdi <balint@codigoergosum.com>
; http://codigoergosum.com

(ns clojure_exercises.factor-9.factor-9
  (:use [criterium.core :as cr :only [with-progress-reporting bench]])
  (:use [clojure.contrib.combinatorics :only [permutations]])
  (:use [clojure.test :only [deftest is]]))

; It is poor form to (:use clojure.string). Instead, use require
; with :as to specify a prefix, e.g.

; (ns your.namespace.here
;   (:require '[clojure.string :as str]))

(defn divisible-by-length [x]
  (zero? (mod x (count (str x)))))

    (defn factor-9
      ([]
        (let [digits (take 9 (iterate #(inc %) 1))
              nums (map (fn [x] ,(Integer. (apply str x))) (permutations digits))]
          (some (fn [x] (and (factor-9 x) x)) nums)))
      ([n]
          (or
            (= 1 (count (str n)))
            (and (divisible-by-length n) (factor-9 (quot n 10))))))

;        (if (divisible-by-length n)
;          (recur (quot n 10))
;          false))))
;(deftest test-divisible-by-length
;  (is (divisible-by-length 2))
;  (is (not (divisible-by-length 21)))
;  (is (divisible-by-length 213))
;  (is (not (divisible-by-length 214))))

;(run-tests)
;(cr/with-progress-reporting (cr/bench (factor-9) :verbose)) ; 381654729
(factor-9)
; ---------------------
;(defn has-all-digits
;  ([x]
;    (has-all-digits x []))
;  ([x seen-digits]
;    (let [next-digit (rem x 10) other-digits (quot x 10)]
;      (or (= 9 (count seen-digits))
;        (and
;          (not (zero? next-digit))
;          (not (some #(= % next-digit) seen-digits))
;          (has-all-digits other-digits (cons next-digit seen-digits)))))))
;
;(deftest test-has-all-digits
;  (is (has-all-digits 123456789))
;  (is (not (has-all-digits 123456780)))
;  (is (has-all-digits 783192546))
;  (is (not (has-all-digits 888888887))))

