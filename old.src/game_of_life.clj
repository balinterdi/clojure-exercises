(ns ^{:doc "Conway Game of Life"
  :author "Balint Erdi <me@balinterdi.com>"}
  clojure_exercises.game_of_life)

; block is a still life
(def block [[0 0 0 0] [0 1 1 0] [0 1 1 0] [0 0 0 0]])
; blinker is an oscillator
(def blinker [[0 0 0 0 0] [0 0 1 0 0] [0 0 1 0 0] [0 0 1 0 0] [0 0 0 0 0]])

(defn display [table]
  (doseq [row table]
    (binding [*out* *err*]
      (let [values (map #(last %) row)]
        (println (apply str (map #(if (zero? %) \- \x) values))))))
  (flush))

; Every cell is represented as a vector:
; [row, column, value]
; E.g [2, 0, 1] is the 3rd row, 1st column and there is life there
(defn add-coordinates [table]
  (let [table-size (count (first table))
        indexes-and-values
          (interleave
            (iterate #(map inc %) (repeat table-size 0))
            (repeat table-size (range 0 table-size))
            table)]
    (mapcat #(partition 3 (apply interleave %)) (partition 3 indexes-and-values))))

(defn to-table [lst table-size]
  (partition table-size lst))

(defn between? [x a b]
  (and (>= x a) (<= x b)))

(defn next-life [table]
  (map
    (fn [[row col value]]
      (let [neighbors
        (filter
          (fn [[nrow ncol _]]
            (and
              (not (and (= nrow row) (= ncol col)))
              (between? nrow (dec row) (inc row))
              (between? ncol (dec col) (inc col))))
          table)
        living (reduce + (map #(last %) neighbors))
        next-value (cond (< living 2) 0 (> living 3) 0 (= value 1) 1 :else (if (= living 2) 0 1))]
      [row col next-value]))
    table))

(defn run [seed]
  (loop [life (add-coordinates seed)]
    (let [table-size (int (Math/sqrt (count life)))]
      (display (to-table life table-size))
      (when (not (= (read-line) "q"))
        (recur (next-life life))))))

; TODOS:
; - write display to a web page (canvas + cloj, script?)
