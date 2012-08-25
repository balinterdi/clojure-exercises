; A simplistic LZW (de)compressor
; author: Balint Erdi <balint@codigoergosum.com>
; http://codigoergosum.com

; The algorythm I implemented is the one described in wikipedia
; http://en.wikipedia.org/wiki/LZW
; It is not for practical use, this was a self-assignment to
; get to know Clojure better

; TODO: read data from algorythm_desc.txt and compress that
; (see pg. 129 in the book), sg like:
; (with-open [rdr (reader "algorythm_desc.txt")]
;  (line-seq rdr))

(ns lzw.compressor
  (:use clojure.contrib.test-is)
  (:use [clojure.contrib.seq-utils :only (includes?)])
  (:use [clojure.contrib.str-utils2 :only (split)]
  (:use [clojure.contrib.duck-streams :only (reader)]  )
)

(defn log2 [x]
  (/ (Math/log x) (Math/log 2)))

(def powers-of-two (cons 0 (iterate (fn [x] (* x 2)) 1)))
(defn power-of-two? [x]
  (= x (nth powers-of-two (inc (log2 x)))))

(defn to-binary
  ([number]
    (apply str (to-binary number [])))
  ([number digits]
    (if
      (or (= 0 number) (= 1 number)) (cons number digits)
      (recur (quot number 2) (cons (rem number 2) digits))))
)

(defn required-dict-key-length [dict]
  (Math/ceil (log2 (count dict))))

(defn get-dict-length [dict key_or_value]
  (let [[ip_block op_block] (first dict)]
    (if (= key_or_value :key)
      (count ip_block)
      (count op_block)))
)

(defn str-split [string]
  (let [[split _] (split-at (count string) string)] split))

(defn pad [string pad_char padded_length]
  (str (reduce str (take (- padded_length (count string)) (repeat pad_char))) string))

(defn vectorize-dict-keys [dict]
  (reduce conj {} (map (fn [[key value]] {(vec key) value}) dict)))

(defn invert-map [mep]
  (reduce conj {} (map (fn [[key value]] {value key}) mep))
)

(def alphabet "#ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def initial_dict
  (let [key_length (required-dict-key-length (str-split alphabet))
        dict_stream (map (fn [letter number] {(list letter) (pad (to-binary number) "0" key_length)}) (str-split alphabet) (iterate inc 0))]
    (reduce conj
      (take (count alphabet) dict_stream))
  )
)

(defn prefixes [lst]
  (take (count lst) (for [i (iterate inc 1)] (take i lst)))
)

(defn longest-encodeable-block [text dict]
  (let [text_prefixes (prefixes text)
        longest_block (last (take-while #(dict %) text_prefixes))]
    [(apply str longest_block), (if (= text longest_block) nil (nth text_prefixes (count longest_block)))]
  )
)

(defn next-bin-code [dict]
  (pad (str (to-binary (count dict))) "0" (required-dict-key-length dict)))

(defn dict-needs-expansion-on? [dict key_or_value]
  (= (Math/pow 2 (get-dict-length dict key_or_value)) (count dict)))

(defn expand-dict [dict what]
  (reduce into (map
                  (fn [[input_block output_block]]
                    (if (= :key what)
                        {(str "0" input_block) output_block}
                        {input_block (str "0" output_block)}))
                  dict))
)

(defn dict-with-new-entry
  [dict new_elt key_or_value]
  (if (or (nil? new_elt) (and (= key_or_value :key) (dict new_elt))) dict
    (let [next_code (next-bin-code dict)
          new_entry (if (= key_or_value :key) {new_elt next_code} {next_code new_elt})
          entity_to_expand (if (= key_or_value :key) :value :key)
          new_dict (if (dict-needs-expansion-on? dict entity_to_expand) (expand-dict dict entity_to_expand) dict)]
      (conj new_dict new_entry))
  )
)

(defn get-next-block [text dict]
  (let [key_length (apply max (map count (keys dict)))]
    [(apply str (take key_length text)), (apply str (drop key_length text))]))

(defn compress
  ([text]
    (str (apply str (compress (str-split text) initial_dict []))))
  ([text dict]
    (str (apply str (compress (str-split text) dict []))))
  ([text dict encoded_text]
    (if (empty? text) encoded_text
      (let [[block, new_entry] (longest-encodeable-block text dict)
        encoded_block (dict (str-split block))
        remaining_text (drop (count block) text)
        new_dict (dict-with-new-entry dict new_entry :key)
        ]
        (recur remaining_text new_dict (conj encoded_text encoded_block))))
  )
)

(defn decompress
  ([text dict]
    (apply str (reduce into [] (decompress text (invert-map (vectorize-dict-keys dict)) []))))
  ([text dict decompressed_text]
    (if (empty? text) decompressed_text
      (let [[block_to_decode, remaining_text] (get-next-block text dict)
          decoded_block (dict block_to_decode)
          ; making a dict. with a dummy block ([?]) to get the proper input block size
          ; the real block to be inserted can only be found out thusly
          expanded_dict (dict-with-new-entry dict [\?] :value)
          [next_block, _] (get-next-block remaining_text expanded_dict)
          new_entry (conj decoded_block (first (expanded_dict next_block)))
          new_dict (dict-with-new-entry dict new_entry :value)
          ]
        (recur remaining_text new_dict (conj decompressed_text decoded_block)))
    )
  )
)

(deftest test-to-binary
  (is (= (to-binary 0) "0"))
  (is (= (to-binary 1) "1"))
  (is (= (to-binary 2) "10"))
  (is (= (to-binary 7) "111"))
  (is (= (to-binary 8) "1000"))
  (is (= (to-binary 77) "1001101"))
)

(deftest test-pad
  (is (= (pad "1" "0" 5) "00001"))
  (is (= (pad "1" "0" 2) "01"))
  (is (= (pad "" "0" 5) "00000"))
  (is (= (pad "10010" "0" 5) "10010"))
  (is (= (pad "10010" "0" 2) "10010"))
)

(deftest test-invert-map
  (is (= (invert-map {}) {}))
  (is (= (invert-map {:one "egy"}) {"egy" :one}))
  (is (= (invert-map {:one "egy", :two "ketto", :three "harom"}) {"egy" :one, "ketto" :two, "harom" :three}))
)

(deftest test-prefixes
  (is (= () (prefixes ())))
  (let [pfxs (prefixes '(\A \A \B \C \C \D))]
    (is (= '((\A) (\A \A)) (take 2 pfxs)))
    (is (= '((\A) (\A \A) (\A \A \B)) (take 3 pfxs)))
  )
)

(deftest test-longest-encodeable-block
  (let [[longest_block, new_entry] (longest-encodeable-block '(\A \B \C) {'(\A) "1", '(\C) "2", '(\Z) "3"})]
    (is (= "A" longest_block))
    (is (= '(\A \B) new_entry)))
  (let [[longest_block, new_entry] (longest-encodeable-block '(\A \A \B \C) {'(\A) "1", '(\A \A) "4", '(\C) "2", '(\Z) "3"})]
    (is (= "AA" longest_block))
    (is (= '(\A \A \B) new_entry)))
  (let [[longest_block, new_entry] (longest-encodeable-block '(\A \A \B \C) {'(\A) "1", '(\A \A) "4", '(\A \A \B) "5", '(\C) "2", '(\Z) "3"})]
    (is (= "AAB" longest_block))
    (is (= '(\A \A \B \C) new_entry)))
  (let [[longest_block, new_entry] (longest-encodeable-block '(\O \T \#) {'(\O \T) "100010", '(\O) "001111", '(\#) "000000"})]
    (is (= "OT" longest_block))
    (is (= '(\O \T \#) new_entry)))
  (let [[longest_block, new_entry] (longest-encodeable-block '(\#) {'(\#) "000000"})]
    (is (= "#" longest_block))
    (is (= nil new_entry)))
)

(deftest test-dict-with-new-entry
  (is (= {'(\A) "0" '(\B) "1"} (dict-with-new-entry {'(\A) "0"} '(\B) :key)))
  (is (= {'(\A) "00" '(\B) "01" '(\C) "10"} (dict-with-new-entry {'(\A) "0" '(\B) "1"} '(\C) :key)))
  (is (= {'(\A) "0" '(\B) "1"} (dict-with-new-entry {'(\A) "0" '(\B) "1"} '(\A) :key)))
  (is (= {'(\A) "0" '(\B) "1"} (dict-with-new-entry {'(\A) "0" '(\B) "1"} nil :key)))
  (is (= {"0" '(\A), "1" '(\B)} (dict-with-new-entry {"0" '(\A)} '(\B) :value)))
  (is (= {"00" '(\A), "01" '(\B), "10" '(\C)} (dict-with-new-entry {"0" '(\A), "1" '(\B)} '(\C) :value)))
)


(deftest test-compress
  (is (= (compress "#") "00000"))
  (is (= (compress "TOBEORNOTTOBEORTOBEORNOT#")
     (str "10100" "01111" "00010" "00101" "01111" "10010" "001110" "001111" "010100" "011011" "011101" "011111" "100100" "011110" "100000" "100010" "000000")))
)

(deftest test-decompress
  (is (= "#" (decompress "00000" initial_dict)))
  (is (= "TOBEORNOTTOBEORTOBEORNOT#"
      (decompress (str "10100" "01111" "00010" "00101" "01111" "10010" "001110" "001111" "010100" "011011" "011101" "011111" "100100" "011110" "100000" "100010" "000000") initial_dict)
  ))
)

(deftest test-process
  (let [original_text "TOBEORNOTTOBEORTOBEORNOT#"
        compressed (compress original_text initial_dict)
        decompressed (decompress compressed initial_dict)]
        (is (= decompressed original_text)))
)

(run-tests)
