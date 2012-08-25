(ns sudoku.helpers)

(defn print-table [table]
  (partition 9 (for [cell (range 81)] (table cell)))
)