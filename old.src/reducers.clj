(ns clojure_exercises.reducers
  (:require [clojure.core.reducers :as r])
  (:require [clojure.string :as s])
  (:require [criterium.core :as c]))

(defn count-letters [word]
  (let [inc-freq
    (fn [freqs c]
      (if (freqs c) (assoc freqs c (inc (freqs c))) (assoc freqs c 1)))]
    (reduce inc-freq {} (s/lower-case word))))

(defn read-words []
  (with-open [rdr (clojure.java.io/reader "/usr/share/dict/words")]
    ; line-seq reads the content of the file as a lazy-seq
    (doseq [w (map count-letters (take 20 (line-seq rdr)))] (println w))))
    ;(println (mapcat count-letters (take 20 (line-seq rdr))))))

(defn letter-distr []
  (with-open [rdr (clojure.java.io/reader "/usr/share/dict/words")]
    (reduce
     (fn [letter-counts word-letter-counts]
         (merge-with + word-letter-counts letter-counts))
     {}
     (map count-letters (line-seq rdr)))))

(defn letter-distr-eager []
  (let [words (s/split (slurp "/usr/share/dict/words") #"\n")
        word-letter-counts (map count-letters words)]
    (reduce
     (fn [letter-counts word-letter-counts]
         (merge-with + word-letter-counts letter-counts))
     word-letter-counts)))

(defn letter-distr-reducers []
  ; words will be split into groups of n
  ; and these groups will be reduced by reducef
  ; the results of these reductions will then be reduced by combinef
  ; (combinef) will be used to produce the seed values for the reductions
  (let [words (s/split (slurp "/usr/share/dict/words") #"\n")
        letter-counts (map count-letters words)
        merge-freqs
          (fn
            ([] {})
            ([lc1 lc2]
             (merge-with + lc1 lc2)))]
    (r/fold merge-freqs letter-counts)))

(defn benchmark-lazy []
  (c/report-result (c/quick-bench (letter-distr)) :verbose))

(defn benchmark-eager []
  (c/report-result (c/quick-bench (letter-distr-eager)) :verbose))

(defn benchmark-reducers []
  (c/report-result (c/quick-bench (letter-distr-reducers)) :verbose))

(defn sort-letters-by-freq [letters]
  (into (sorted-map-by (fn [l1 l2]
    (compare [(letters l2) l2] [(letters l1) l1])))
    letters))

;(benchmark-lazy) execution time mean: 6.76 sec
;(benchmark-reducers) ; execution time mean: 2.67 sec
;(benchmark-eager) ; execution time mean: 6.23 sec
