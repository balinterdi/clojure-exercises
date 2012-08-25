(ns sudoku.game
  (:use clojure.contrib.test-is)
  (:use sudoku.examples)
  (:use sudoku.helpers)
  (:use seqs))

(def table-size 9)
(def grid-size 3)
(def num-grids (quot table-size grid-size))
(def the-numbers (range 1 (inc table-size)))
(def num-of-cells (* table-size table-size))

(defn column-for [cell]
  (mod cell table-size)
)

(defn row-for [cell]
  (quot cell table-size)
)

(defn grid-leaders []
  ;TODO: there must be an easier way to have a (0 0 0 3 3 3 6 6 6) sequence...
  (let [row-starts (nth (repeat-changing 0 3 grid-size) (dec grid-size))
        col-starts (cycle (range 0 table-size grid-size))
        grid-leader-pairs (map (fn [row_idx col_idx]
                                  {[row_idx col_idx] (+ (* row_idx table-size) col_idx)})
                                row-starts col-starts)]
    (reduce (fn [p1 p2] (conj p1 p2)) grid-leader-pairs)
  )
)

(defn grid-leader-for [cell]
  ;TODO: memoization for grid-leaders would be cool so that it's not recalculated each time
  (let [gl-row (* (quot (row-for cell) grid-size) grid-size)
        gl-column (* (quot (column-for cell) grid-size) grid-size)]
    ((grid-leaders) (vector gl-row gl-column))
  )
)

(defn numbers-between [base step from to]
  (if (or (< base from) (> base to)) ()
    (into (range base to step) (rest (range base (dec from) (* -1 step))))
  )
)

(defn row-neighbors [cell]
  (let [nums (numbers-between cell 1 (* table-size (quot cell table-size)) (* table-size (inc (quot cell table-size))))]
    (remove #(= cell %) nums)
  )
)

(defn column-neighbors [cell]
  (let [nums (numbers-between cell table-size 0 (dec (* table-size table-size)))]
    (remove #(= cell %) nums)
  )
)

(defn grid-neighbors [cell]
  (let [gl (grid-leader-for cell)
        row-starts (nth (repeat-changing 0 grid-size table-size) (dec grid-size))
        col-starts (cycle (range 0 grid-size))
        neighbors-and-me (map + (repeat gl) row-starts col-starts)
    ]
    (remove #(= cell %) neighbors-and-me)
  )
)

(defn neighbors [cell]
  (set (concat (row-neighbors cell)
               (column-neighbors cell)
             (grid-neighbors cell)))
)

(defn missing-from-row [coll]
  (filter #(not ((set coll) %)) (range 1 (inc table-size)))
)

(defn numbers-in-row [cell table]
  (for [n (row-neighbors cell) :when (not (nil? (table n)))] (table n))
  ;(filter #(not (nil? %)) (map #(table %) neighbors))
)

(defn numbers-in-column [cell table]
  (for [n (column-neighbors cell) :when (not (nil? (table n)))] (table n))
)

;(defn numbers-in-grid [cell table])

(defn make-sudoku-table [cells]
  cells
)

(defn neighbor-numbers [cell table]
  (for [n (neighbors cell) :when (not (nil? (table n)))] (table n))
)

(defn possible-numbers-for [cell table]
  (if (not (nil? (table cell))) []
    ; remove the numbers that appear as any neighbor of cell
    (for [num the-numbers :when (not (some #(= num %) (neighbor-numbers cell table)))] num)
  )
)

(defn valid-table? [table]
  ; a table is valid if none of the numbers has the same number as a neighbor
  (every? #(not (member? (table %) (neighbor-numbers % table))) (keys table))
)

(defn finished? [table]
  (= (count table) num-of-cells)
)

(defn empty-cells [table]
  (filter #(nil? (table %)) (range 0 num-of-cells))
)

(defn next-possible-moves [table]
  (categorize #(count (possible-numbers-for % table)) (empty-cells table))
)

(defn two-possible-numbers [table]
  ((next-possible-moves table) 2)
)

(defn guess-next-step [table]
  (let [cell (first (two-possible-numbers table))
      numbers (possible-numbers-for cell table)]
    [{cell (first numbers)} cell (rest numbers)]
  )
)

(defn sure-steps [table]
  (for [sure_cell ((next-possible-moves table) 1)] {sure_cell (first (possible-numbers-for sure_cell table))})
)

(defn table-with-sure-steps [table]
  (let [moves (next-possible-moves table) sure_moves (moves 1)]
    (into table (for [cell sure_moves] {cell (first (possible-numbers-for cell table))}))
  )
)

; returns the next-move on table
; if there is a cell where nothing can be written -> return nil
; if there is at least one unambiguous cell -> return that
; if a guess needs to be made, than return the guess, the cell and other possibilities for that cell
(defn next-move [table]
  (let [next_moves (next-possible-moves table)]
    (if
      (not (empty? (next_moves 0))) [nil nil [] nil]
      (let [smallest_liberty (first (sort (keys next_moves)))
          cell (first (next_moves smallest_liberty))
          numbers_for_cell (possible-numbers-for cell table)]
          [{cell (first numbers_for_cell)} cell (rest numbers_for_cell) smallest_liberty]
      )
    )
  )
)

(defn solve [table cell_under_guess cell_other_posss]
  (if (finished? table)
    table
    (let [[move cell other_posss smallest_lib] (next-move table)]
      (if (nil? move)
        nil
        (if (= smallest_lib 1)
          (solve (into table move) cell_under_guess cell_other_posss)
          (let [solved (solve (into table move) cell other_posss)]
            (if (nil? solved)
              (let [next_guess {cell (first other_posss)}]
                (solve (into table next_guess) cell (rest other_posss))
              )
              solved
            )
          )
        )
      )
    )
  )
)

(defn generate-invalid-complete-table []
  (reduce (fn [m n] (conj m n)) (for [n (range 0 num-of-cells)] {n (inc (rand-int table-size))}))
)

(deftest test-numbers-between
  (is (= [0 10 20 30 40 50 60 70 80] (numbers-between 10 10 0 90)))
  (is (= [0 10 20 30 40 50 60 70 80] (numbers-between 70 10 0 90)))
  (is (= [7 17 27 37 47 57 67 77 87] (numbers-between 7 10 0 90)))
  (is (= [9 22 35 48 61 74 87] (numbers-between 61 13 0 100)))
  (is (= [] (numbers-between 7 2 0 5)))
  (is (= [0 1 2 3 4] (numbers-between 3 1 0 5)))
)

(deftest test-missing-from-row
  (is (= [2] (missing-from-row [4 3 7 8 1 9 5 6])))
  (is (= (set [2 6]) (set (missing-from-row [4 3 7 8 1 9 5]))))
  (is (= (set (range 1 10)) (set (missing-from-row []))))
)

(deftest test-numbers-in-row
  (is (= [] (numbers-in-row 0 (make-sudoku-table {}))))
  (is (= (set [7]) (set (numbers-in-row 0 (make-sudoku-table {0 1, 1 7})))))
  (is (= (set [7 2 9]) (set (numbers-in-row 0 (make-sudoku-table {1 7, 5 2, 7 9})))))
  (is (= (set [7 2 9]) (set (numbers-in-row 0 (make-sudoku-table {0 1, 1 7, 5 2, 7 9})))))
  (is (= (set []) (set (numbers-in-row 9 (make-sudoku-table {0 1, 9 4, 20 2, 70 8})))))
  (is (= (set [7 8]) (set (numbers-in-row 9 (make-sudoku-table {0 1, 9 4, 11 8, 17 7, 20 2, 70 8})))))
)

(deftest test-numbers-in-column
  (is (= [] (numbers-in-column 0 (make-sudoku-table {}))))
  (is (= [] (numbers-in-column 0 (make-sudoku-table {1 1, 2 2, 3 3, 4 4, 5 5, 6 6, 7 7, 8 8}))))
  (is (= (set []) (set (numbers-in-column 0 (make-sudoku-table {0 1, 1 7})))))
  (is (= (set [7]) (set (numbers-in-column 0 (make-sudoku-table {0 1, 9 7})))))
  (is (= (set [4 9 7]) (set (numbers-in-column 0 (make-sudoku-table {0 1, 1 7, 27 4, 54 9, 63 7, 70 4})))))
  (is (= (set [2 8 3 7 5]) (set (numbers-in-column 13 (make-sudoku-table {0 1, 4 2, 14 7, 22 8, 34 8, 42 2, 40 3, 49 7, 57 9, 76 5})))))
)

;(deftest test-numbers-in-grid
;  (is (= [] (numbers-in-grid 0 {})))
;  (is (= [1] (numbers-in-grid 0 {0 1})))
;  (is (= (set [1 2 5]) (numbers-in-grid 0 {0 1 10 5 19 2})))
;  (is (= (set [3 6 7]) (numbers-in-grid 78 {0 1 22 7 24 8 37 2 61 6 62 7 71 3})))
;)

(deftest test-row-neighbors
  (is (= [1 2 3 4 5 6 7 8] (row-neighbors 0)))
  (is (= [0 2 3 4 5 6 7 8] (row-neighbors 1)))
  (is (= [0 1 2 3 4 6 7 8] (row-neighbors 5)))
  (is (= [46 47 48 49 50 51 52 53] (row-neighbors 45)))
  (is (= [45 46 47 49 50 51 52 53] (row-neighbors 48)))
  (is (= [45 46 47 48 49 50 51 52] (row-neighbors 53)))
)

(deftest test-column-neighbors
  (is (= [9 18 27 36 45 54 63 72]) (column-neighbors 0))
  (is (= [0 9 18 36 45 54 63 72]) (column-neighbors 27))
  (is (= [0 9 18 27 36 45 54 63]) (column-neighbors 72))
  (is (= [4 22 31 40 49 58 67 76]) (column-neighbors 13))
  (is (= [4 13 22 31 40 49 58 76]) (column-neighbors 67))
  (is (= [4 13 22 31 40 49 58 67]) (column-neighbors 76))
)

(deftest test-grid-neighbors
  (is (= [1 2 9 10 11 18 19 20] (grid-neighbors 0)))
  (is (= [0 1 2 9 11 18 19 20] (grid-neighbors 10)))
  (is (= [0 1 2 9 10 11 18 20] (grid-neighbors 19)))
  (is (= [0 1 2 9 10 11 18 19] (grid-neighbors 20)))
  (is (= [31 32 39 40 41 48 49 50] (grid-neighbors 30)))
  (is (= [30 31 39 40 41 48 49 50] (grid-neighbors 32)))
  (is (= [30 31 32 40 41 48 49 50] (grid-neighbors 39)))
  (is (= [30 31 32 39 40 41 48 50] (grid-neighbors 49)))
  (is (= [57 58 59 66 67 68 75 76] (grid-neighbors 77)))
)

(deftest test-neighbors
  (is (= (set [0 1 2 3 4 5 6 7 8 9 18 27 36 45 54 63 72 10 11 19 20])) (neighbors 0))
  (is (= (set [0 1 2 3 4 5 6 7 8 10 19 28 37 46 55 64 73 9 11 18 20])) (neighbors 1))
  (is (= (set [36 37 38 39 40 41 42 43 44 4 13 22 31 49 58 67 76 30 32 48 50])) (neighbors 41))
  (is (= (set [54 55 56 57 58 59 60 61 62 8 17 26 35 44 53 71 80 69 70 78 79])) (neighbors 62))
)

(deftest test-grid-leaders
  (is (= {[0 0] 0 [0 3] 3 [0 6] 6 [3 0] 27 [3 3] 30 [3 6] 33 [6 0] 54 [6 3] 57 [6 6] 60} (grid-leaders)))
)

(deftest test-column-for
  (is (= 0 (column-for 0)))
  (is (= 0 (column-for 18)))
  (is (= 3 (column-for 3)))
  (is (= 3 (column-for 30)))
  (is (= 5 (column-for 32)))
  (is (= 1 (column-for 73)))
  (is (= 8 (column-for 80)))
)

(deftest test-row-for
  (is (= 0 (row-for 0)))
  (is (= 0 (row-for 7)))
  (is (= 1 (row-for 9)))
  (is (= 3 (row-for 30)))
  (is (= 5 (row-for 47)))
  (is (= 6 (row-for 62)))
  (is (= 8 (row-for 80)))
)

(deftest test-grid-leader-for
  (is (= 0 (grid-leader-for 0)))
  (is (= 0 (grid-leader-for 2)))
  (is (= 0 (grid-leader-for 10)))
  (is (= 0 (grid-leader-for 19)))
  (is (= 30 (grid-leader-for 30)))
  (is (= 30 (grid-leader-for 40)))
  (is (= 30 (grid-leader-for 50)))
  (is (= 57 (grid-leader-for 77)))
  (is (= 60 (grid-leader-for 80)))
)

(deftest test-make-sudoku-table
  (let [sudoku_table (make-sudoku-table {0 1, 1 7, 5 2, 7 9})]
    (is (= 1 (sudoku_table 0)))
    (is (= 7 (sudoku_table 1)))
    (is (= 2 (sudoku_table 5)))
    (is (= 9 (sudoku_table 7)))
    (is (= nil (sudoku_table 10)))
    (is (= nil (sudoku_table 20)))
    (is (= nil (sudoku_table 89)))
  )
)

(deftest test-neighbor-numbers
  (is (= [] (neighbor-numbers 0 (make-sudoku-table {}))))
  (is (= [] (neighbor-numbers 0 (make-sudoku-table {0 1}))))
  (is (= [7] (neighbor-numbers 0 (make-sudoku-table {0 1, 1 7}))))
  (is (= (set [7 8 3]) (set (neighbor-numbers 0 (make-sudoku-table {0 1, 1 7, 5 8, 9 3})))))
  (is (= (set [1 8 3]) (set (neighbor-numbers 1 (make-sudoku-table {0 1, 1 7, 5 8, 9 3})))))
  (is (= (set [1 7]) (set (neighbor-numbers 5 (make-sudoku-table {0 1, 1 7, 5 8, 9 3})))))
  (is (= (set [1 7]) (set (neighbor-numbers 9 (make-sudoku-table {0 1, 1 7, 5 8, 9 3})))))
  (is (= (set [1 2 3 4 5 6 7 8]) (set (neighbor-numbers 80 (make-sudoku-table {60 1, 61 2, 62 3, 69 4, 70 5, 71 6, 78 7, 79 8})))))
)

(deftest test-possible-numbers-for
  (is (= (set the-numbers) (set (possible-numbers-for 0 (make-sudoku-table {})))))
  (is (= (set []) (set (possible-numbers-for 0 (make-sudoku-table {0 1})))))
  (is (= (set [2 3 4 5 6 7 8 9]) (set (possible-numbers-for 1 (make-sudoku-table {0 1})))))
  (is (= (set [2 3 4 5 6 8 9]) (set (possible-numbers-for 2 (make-sudoku-table {0 1, 1 7})))))
)

(deftest test-valid-table?
  (is (true? (valid-table? (make-sudoku-table {}))))
  (is (true? (valid-table? (make-sudoku-table {0 1}))))
  (is (true? (valid-table? (make-sudoku-table {0 1, 1 2, 2 3, 3 4, 4 5, 5 6, 6 7, 7 8, 8 9}))))
  (is (true? (valid-table? (make-sudoku-table {0 1, 10 2, 20 3, 30 4, 40 5, 50 6, 60 7, 70 8, 80 9, 8 4, 16 9, 24 2, 32 3, 48 1, 56 6, 64 7, 72 8}))))
  (is (false? (valid-table? (make-sudoku-table {0 1, 10 2, 20 3, 30 4, 40 5, 50 6, 60 7, 70 8, 80 9, 8 4, 16 9, 24 2, 32 3, 48 1, 56 6, 64 7, 72 8, 1 1}))))
  (is (false? (valid-table? (make-sudoku-table {0 1, 10 2, 20 3, 30 4, 40 5, 50 6, 60 7, 70 8, 80 9, 8 4, 16 9, 24 2, 32 3, 48 1, 56 6, 64 7, 72 8, 1 2}))))
  (is (false? (valid-table? (make-sudoku-table {0 1, 10 2, 20 3, 30 4, 40 5, 50 6, 60 7, 70 8, 80 9, 8 4, 16 9, 24 2, 32 3, 48 1, 56 6, 64 7, 72 8, 1 3}))))
  (is (false? (valid-table? (make-sudoku-table {0 1, 10 2, 20 3, 30 4, 40 5, 50 6, 60 7, 70 8, 80 9, 8 4, 16 9, 24 2, 32 3, 48 1, 56 6, 64 7, 72 8, 76 5}))))
  (is (true? (valid-table? easy_table)))
  (is (true? (valid-table? pro_table)))
)

(deftest test-finished?
  (is (false? (finished? (make-sudoku-table {}))))
  (is (false? (finished? (make-sudoku-table {0 1, 3 7}))))
  (is (false? (finished? (make-sudoku-table easy_table))))
  (is (true? (finished? (make-sudoku-table (generate-invalid-complete-table)))))
)

(deftest test-next-possible-moves
  (let [moves (next-possible-moves easy_table)]
    (is (true? (member? 30 (moves 2))))
    (is (true? (member? 60 (moves 1))))
    (is (true? (member? 75 (moves 2))))
    (is (true? (member? 21 (moves 3))))
    (is (true? (member? 59 (moves 2))))
  )
)

(deftest test-empty-cells
  (is (= (range 0 num-of-cells) (empty-cells (make-sudoku-table {}))))
  (is (= [] (empty-cells (make-sudoku-table (generate-invalid-complete-table)))))
  (is (= (drop-while #(<= % 3) (range 0 num-of-cells)) (empty-cells (make-sudoku-table {0 1, 1 2, 2 3, 3 4}))))
)

(deftest test-sure-steps
  (is (= (set (sure-steps easy_table)) (set '({40 6} {60 3} {61 9} {62 1} {67 8} {73 1} {74 2}))))
)

(deftest test-table-with-sure-steps
  ; my hope is that this is how you stub methods in an FP language :)
  ;(let [next-possible-moves (fn [table steps_taken] {1 [23 76], 2 [12 24 36], 3 [33 44 61], 4 [7 12 67]})
  ;      possible-numbers-for (fn [cell table] (cond (= cell 23) 5 (= cell 76) 3 :true 7))]
  ;  (is (= {0 4, 13 5, 23 5, 76 3, 26 7, 28 9} (table-with-sure-steps {0 4, 13 5, 26 7, 28 9})))
  ;)
  (is (= {0 7, 32 5, 1 8, 2 5, 34 3, 66 1, 67 8, 4 1, 68 3, 38 8, 39 7, 71 2, 8 9, 40 6, 72 3, 9 1, 41 1, 73 1, 42 9, 74 2, 12 2, 76 4, 14 7, 46 2, 78 6, 79 7, 48 8, 80 8, 17 3, 49 3, 51 1, 22 5, 54 8, 23 8, 55 5, 24 7, 56 4, 25 1, 57 6, 26 6, 58 7, 60 3, 29 1, 61 9, 62 1, 31 2, 63 9} (table-with-sure-steps easy_table)))
)

(deftest test-two-possible-numbers
  (is (= (two-possible-numbers (generate-invalid-complete-table)) nil))
  (is (= (two-possible-numbers easy_table) [3 5 6 7 11 13 18 27 30 33 44 59 64 65 69 70 75 77]))
)

(deftest test-guess-next-step
  (let [no_more_sure_moves_table {0 7, 32 5, 64 6, 1 8, 65 7, 2 5, 34 3, 66 1, 3 3, 67 8, 4 1, 68 3, 5 6, 37 3, 38 8, 39 7, 71 2, 8 9, 40 6, 72 3, 9 1, 41 1, 73 1, 10 4, 42 9, 74 2, 11 6, 75 5, 12 2, 76 4, 13 9, 77 9, 14 7, 46 2, 78 6, 47 9, 79 7, 48 8, 80 8, 17 3, 49 3, 18 2, 50 4, 19 9, 51 1, 20 3, 21 4, 22 5, 54 8, 23 8, 55 5, 24 7, 56 4, 25 1, 57 6, 26 6, 58 7, 59 2, 28 7, 60 3, 29 1, 61 9, 30 9, 62 1, 31 2, 63 9}]
    (is (= (guess-next-step no_more_sure_moves_table) [{6 2} 6 [4]]))
  )
)

(deftest test-next-move
  (let [[move cell other_posss smallest_liberty] (next-move unsolveable_pro_table)]
    (is (nil? move))
  )
  (let [[move cell other_posss smallest_liberty] (next-move easy_table)]
    (= smallest_liberty 1)
  )
  (let [[move cell other_posss smallest_liberty] (next-move pro_table)]
    (= smallest_liberty 2)
  )
  (let [[move cell other_posss smallest_liberty] (next-move {})]
    (= smallest_liberty table-size)
  )
)

(deftest test-solve
  ;(is (= (solve easy_table nil []) {0 7, 32 5, 64 6, 1 8, 33 8, 65 7, 2 5, 34 3, 66 1, 3 3, 35 4, 67 8, 4 1, 36 4, 68 3, 5 6, 37 3, 69 4, 6 2, 38 8, 70 5, 7 4, 39 7, 71 2, 8 9, 40 6, 72 3, 9 1, 41 1, 73 1, 10 4, 42 9, 74 2, 11 6, 43 2, 75 5, 12 2, 44 5, 76 4, 13 9, 45 5, 77 9, 14 7, 46 2, 78 6, 15 5, 47 9, 79 7, 16 8, 48 8, 80 8, 17 3, 49 3, 18 2, 50 4, 19 9, 51 1, 20 3, 52 6, 21 4, 53 7, 22 5, 54 8, 23 8, 55 5, 24 7, 56 4, 25 1, 57 6, 26 6, 58 7, 27 6, 59 2, 28 7, 60 3, 29 1, 61 9, 30 9, 62 1, 31 2, 63 9}))
  ;(is (= (solve pro_table nil []) {0 6, 32 8, 64 6, 1 8, 33 9, 65 4, 2 5, 34 6, 66 2, 3 9, 35 7, 67 8, 4 7, 36 9, 68 7, 5 4, 37 7, 69 1, 6 2, 38 8, 70 5, 7 3, 39 4, 71 9, 8 1, 40 2, 72 8, 9 7, 41 6, 73 5, 10 4, 42 3, 74 7, 11 3, 43 1, 75 1, 12 8, 44 5, 76 4, 13 1, 45 5, 77 9, 14 2, 46 3, 78 6, 15 5, 47 6, 79 2, 16 9, 48 7, 80 3, 17 6, 49 9, 18 1, 50 1, 19 2, 51 8, 20 9, 52 4, 21 3, 53 2, 22 6, 54 2, 23 5, 55 9, 24 4, 56 1, 25 7, 57 6, 26 8, 58 5, 27 4, 59 3, 28 1, 60 7, 29 2, 61 8, 30 5, 62 4, 31 3, 63 3}))
)

(run-tests)