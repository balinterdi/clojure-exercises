(ns seqs
  (:use clojure.contrib.test-is))

(defn repeat-changing [start length step]
  (iterate #(concat % (repeat length (+ step (last %)))) (repeat length start))
)

;NOTE: there is includes? in clojure.contrib which basically does the same
(defn member? [elt coll]
  (some #(= elt %) coll)
)

(defn vectorize-map-values [mep]
  (reduce (fn [m [key value]]
              (conj m {key (if (vector? value) value [value])})
          ) {} mep)
)

(defn merge-maps
  ; merge two maps together non-destructively
  ; the resulting map will have its values in arrays
  ; even if no merge was necessary on its key
  ([] {})
  ([orig_map] orig_map)
  ([orig_map new_map]
    (reduce (fn [m [key value]]
              (let [value_in_m (m key)]
                (if (nil? value_in_m) 
                  (conj m {key [value]})
                  (conj m {key (conj value_in_m value)})
                )
              )
            )
      (vectorize-map-values orig_map) new_map)
  )
)

(defn categorize [func coll]
  ; calls func on each element of coll
  ; and returns a map where the keys are the distinct output values of this call
  ; and the values are the coll items having that output value
  (let [results (for [elt coll] {(func elt) elt})]
    (reduce merge-maps {} results)
  )
)

(deftest test-repeat-changing
  (is (= [0 0 0 9 9 9 18 18 18] (nth (repeat-changing 0 3 9) 2)))
  (is (= [0 0 0 3 3 3 6 6 6] (nth (repeat-changing 0 3 3) 2)))
  (is (= [0 0 1 1 2 2] (nth (repeat-changing 0 2 1) 2)))
)

(deftest test-member?
  (is (nil? (member? "a" [])))
  (is (true? (member? 2 (range 0 5))))
  (is (nil? (member? 5 (range 0 5))))
  (is (true? (member? 5 [34 5 -2 7])))
  (is (nil? (member? "socks" [34 5 -2 7])))
;  (is (true? (member? #{} )))
)

(deftest test-vectorize-map-values
  (is (= {} (vectorize-map-values {})))
  (is (= {:car [:seat]} (vectorize-map-values {:car :seat})))
  (is (= {:car [:seat]} (vectorize-map-values {:car [:seat]})))
  (is (= {:car [:seat] :food [:samosa] :dog [:dixie]} (vectorize-map-values {:car :seat :food :samosa :dog :dixie})))
)

(deftest test-merge-maps
  (is (= {} (merge-maps)))
  (is (= {:food :samosa, :car :bmw} (merge-maps {:food :samosa, :car :bmw})))
  (is (= {} (merge-maps {} {})))
  (is (= {:car [:vw]} (merge-maps {} {:car :vw})))
  (is (= {:car [:vw], :food [:samosa]} (merge-maps {:car :vw} {:food :samosa})))
  (is (= {:car [:vw :seat], :food [:samosa :icecream], :fruit [:apple]} (merge-maps {:car :vw, :food :samosa}, { :car :seat, :food :icecream, :fruit :apple})))
)

(deftest test-categorize
  (is (= {} (categorize #(rem % 3) [])))
  (is (= {1 [1]} (categorize #(rem % 3) [1])))
  (is (= {0 [6 9 12] 1 [1 10 19] 2 [5 8]} (categorize #(rem % 3) [1 5 6 8 9 10 12 19])))
  (is (= {true [1 3 7 11] false [8 18]} (categorize odd? [1 3 7 8 11 18])))
)

(run-tests)