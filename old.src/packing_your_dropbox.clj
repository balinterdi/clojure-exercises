(ns ^{:doc "Packing your Dropbox challenge made up by Dropbox"
  :author "Balint Erdi <balint@balinterdi.com>"}
  clojure_exercises.packing_your_dropbox
  (:use [clojure.contrib.duck-streams :only (reader)])
  (:require [clojure.test :as test :only (deftest is run-tests)])
  (:require [clojure.string :as str :only (split)]))

(defstruct box :bottom-left :top-right)

(def TOP 0)
(def RIGHT 1)

(defn left [abox]
  (first (:bottom-left abox)))

(defn bottom [abox]
  (last (:bottom-left abox)))

(defn right [abox]
  (first (:top-right abox)))

(defn top [abox]
  (last (:top-right abox)))

(defn left-top [abox]
  [(left abox) (top abox)])

(defn right-bottom [abox]
  [(right abox) (bottom abox)])

(defn rand-item [coll]
  (nth coll (rand-int (count coll))))

(defn to-box [[left bottom] [width height]]
  (struct box [left bottom] [(+ left width) (+ bottom height)]))

(defn neighbors [arrangement abox side]
  "Returns all the boxes that touch (are snapped to) box on side"
  (defn touches? [other-box]
    (and (not (= abox other-box))
      (cond
        (= side RIGHT)
          (= (right abox) (left other-box))
        (= side TOP)
          (= (top abox) (bottom other-box))
        :else false)))
  (filter touches? arrangement))

(defn snap-point [arrangement box side]
  "Returns the position of the corner where box can be snapped
  into arrangement on side"
  (let [box-neighbors (neighbors arrangement box side)
        side-snap-fn (if (= side RIGHT) right-bottom left-top)]
    (if (empty? box-neighbors)
      (side-snap-fn box)
      (side-snap-fn (cond
        (= side RIGHT)
          (apply max-key top box-neighbors)
        (= side TOP)
          (apply max-key right box-neighbors))))))

(defn snap [arrangement target box-size side]
  "Snap box to target so that they touch on side"
  (let [snap-here (snap-point arrangement target side)
        new-box (to-box snap-here box-size)]
    (conj arrangement new-box)))

(defn format-input [lines]
  (map #(vec (map (fn [size] (Integer. size)) (str/split % #"\s+"))) lines))

(defn container-for-boxes [boxes]
  "Return the smallest container size (width * height) in which boxes fit"
  (vector (right (apply max-key right boxes)) (top (apply max-key top boxes))))

(defn container-size [boxes]
  (apply * (container-for-boxes boxes)))

(defn overlap? [abox other-box]
  "Return true of abox and other-box overlap, false otherwise"
  (let [between (fn [value _min _max] (and (> value _min) (< value _max)))
        _overlap? (fn [box1 box2]
          (or
            (= box1 box2)
            (and
              (or (between (left box1) (left box2) (right box2))
                  (between (right box1) (left box2) (right box2)))
              (or (between (bottom box1) (bottom box2) (top box2))
                  (between (top box1) (bottom box2) (top box2))))))]
      (or (_overlap? abox other-box) (_overlap? other-box abox))))

(defn valid? [[first-box & rest-boxes]]
  "Return true if input is a valid box arrangement"
  (if (nil? first-box)
    true
    (and
      (nil? (some #(overlap? first-box %) rest-boxes))
      (recur rest-boxes))))

(defn next-arrangements
  "Returns a sequence of the next possible arrangements with new-box"
  ([boxes new-box]
    (next-arrangements boxes boxes new-box []))
  ([[first-box & rest-boxes :as boxes] old-boxes new-box out]
    (if (nil? first-box)
      out
      (let [[box-width box-height] new-box
            snap-options [[new-box RIGHT] [new-box TOP] [[box-height box-width] RIGHT] [[box-height box-width] TOP]]
            valid-new-boxes (filter valid? (for [[box-to-snap side] snap-options] (snap old-boxes first-box box-to-snap side)))]
        (recur rest-boxes old-boxes new-box (concat valid-new-boxes out))))))

(defn transform-for-display
  ([dropbox]
    (transform-for-display dropbox []))
  ([[first-box & rest-dropbox :as dropbox] transformed]
    (if (nil? first-box)
      transformed
      (let [transform-box
        (fn [abox]
          (struct-map box :bottom-left (:bottom-left abox) :top-right [(dec (right abox)) (dec (top abox))]))]
        (recur rest-dropbox (conj transformed (transform-box first-box)))))))

(defn corners [dropbox]
  (distinct
    (apply concat
      (for [box dropbox]
        (list
          (:bottom-left box)
          (:top-right box)
          (right-bottom box)
          (left-top box))))))

(defn print-dropbox [dropbox]
  (let [[width height] (container-for-boxes dropbox)
        corner? (fn [row column] (some #(= % [row column]) (corners dropbox)))
        horizontal-edge? (fn [column row]
                            (some
                              (fn [box]
                                (or
                                  (and
                                    (= (bottom box) row)
                                    (> column (left box))
                                    (< column (right box)))
                                  (and
                                    (= (top box) row)
                                    (> column (left box))
                                    (< column (right box)))))
                              dropbox))
        vertical-edge? (fn [column row]
                            (some
                              (fn [box]
                                (or
                                  (and
                                    (= (left box) column)
                                    (> row (bottom box))
                                    (< row (top box)))
                                  (and
                                    (= (right box) column)
                                    (> row (bottom box))
                                    (< row (top box)))))
                              dropbox))
        mark-for (fn [x y]
                   (cond
                     (corner? x y) "+"
                     (horizontal-edge? x y) "-"
                     (vertical-edge? x y) "|"
                     :else " "))
        printed-rows (for [row (range height -1 -1)]
                       (for [column (range 0 (inc width))]
                         (mark-for column row)))]
      (doseq [row printed-rows]
        (binding [*out* *err*]
          (println (apply str (interpose " " row)))
          (flush)))))

; (print-dropbox arr)
; 11x8:
; + - - - - - - + + - +
; |             | |   |
; |             | |   |
; |             | + - +
; |             | + - +
; |             | |   |
; |             | |   |
; + - - - - - - + + - +

(defn pack
  ([input]
    (let [boxes (format-input input)]
      (pack (rest boxes) [(struct-map box :bottom-left [0 0] :top-right (first boxes))])))
  ([[first-box & rest-boxes] dropbox]
    (if
      (nil? first-box) (or
          (print-dropbox (transform-for-display dropbox))
          (container-size dropbox))
      (let [new-arrangements (next-arrangements dropbox first-box)
            best-arrangement (reduce #(if (< (container-size %1) (container-size %2)) %1 %2) new-arrangements)]
        (recur rest-boxes best-arrangement)))))

(defn corner? [arrangement row column]
  (some
    (fn [box]
      (or (= (:bottom-left box) [row column])
          (= (:top-right box) [row column])
          (= (right-bottom box) [row column])
          (= (left-top box) [row column])))
    arrangement))

(defn edge? [arrangement row column]
  (some
    (fn [box]
      (cond (or
              (= (left box) column)
              (= (right box) column))
            :horizontal
            (or
              (= (bottom box) row)
              (= (top box) row))
            :vertical
            :else nil))
    arrangement))

(test/deftest test-neighbors
  (test/is (=
    (neighbors
      (list (struct-map box :bottom-left [0 0] :top-right [4 10])
            (struct-map box :bottom-left [4 0] :top-right [6 2])
            (struct-map box :bottom-left [0 10] :top-right [2 13])
            (struct-map box :bottom-left [4 2] :top-right [8 3]))
      (struct-map box :bottom-left [0 0] :top-right [4 10])
      RIGHT)
    (list (struct-map box :bottom-left [4 0] :top-right [6 2])
          (struct-map box :bottom-left [4 2] :top-right [8 3])))))

(test/deftest test-overlap?
  (test/is (= false
    (overlap? {:bottom-left [0 0] :top-right [3 3]}
              {:bottom-left [3 0] :top-right [6 3]})))
  (test/is (= false
    (overlap? {:bottom-left [0 0] :top-right [3 3]}
              {:bottom-left [3 3] :top-right [6 6]})))
  (test/is (= true
    (overlap? {:bottom-left [0 0] :top-right [6 6]}
              {:bottom-left [0 0] :top-right [3 3]})))
  (test/is (= true
    (overlap? {:bottom-left [0 0] :top-right [6 6]}
              {:bottom-left [0 0] :top-right [3 3]})))
  (test/is (= true
    (overlap? {:bottom-left [0 0] :top-right [3 3]}
              {:bottom-left [0 0] :top-right [3 3]})))
  (test/is (= true
    (overlap? {:bottom-left [0 0] :top-right [6 6]}
              {:bottom-left [3 0] :top-right [6 3]})))
  (test/is (= true
    (overlap? {:bottom-left [3 3] :top-right [6 6]}
              {:bottom-left [5 5] :top-right [9 9]})))
  (test/is (= true
    (overlap? {:bottom-left [3 3] :top-right [6 6]}
              {:bottom-left [5 5] :top-right [9 9]}))))
; {:bottom-left [11 0], :top-right [17 3]} {:bottom-left [11 0], :top-right [19 5]})

(test/deftest test-valid?
  (test/is (= true
    (valid?
      '({:bottom-left [0 0] :top-right [8 8]}
        {:bottom-left [8 0] :top-right [11 4]}
        {:bottom-left [0 8] :top-right [4 11]}))))
  (test/is (= true
    (valid?
      '({:bottom-left [0 0], :top-right [7 2]}
        {:bottom-left [7 0], :top-right [11 3]}
        {:bottom-left [11 0], :top-right [17 3]}
        {:bottom-left [17 0], :top-right [25 5]}
        {:bottom-left [0 2], :top-right [4 3]}))))
  (test/is (= false
    (valid?
      '({:bottom-left [0 0], :top-right [7 2]}
        {:bottom-left [7 0], :top-right [11 3]}
        {:bottom-left [11 0], :top-right [17 3]}
        {:bottom-left [17 0], :top-right [25 5]}
        {:bottom-left [0 2], :top-right [4 3]}
        {:bottom-left [4 2], :top-right [11 5]})))))

; (test/run-tests)
(defn read-boxes
  ([]
    (read-boxes nil []))
  ([left boxes]
    (cond
      (nil? left) (recur (Integer. (read-line)) [])
      (zero? left) (println (pack boxes))
      :else (recur (dec left) (conj boxes (read-line))))))

; (read-boxes)
(with-open [rdr (reader "/Users/balint/code/clojure/clojure_exercises/pyd_input.txt")]
  ; line-seq reads the content of the file as a lazy-seq
  (println (pack (line-seq rdr))))

; (binding [*out* *err*] (println "foo"))
; (binding [*out* *err*] (print "foo") (flush))

