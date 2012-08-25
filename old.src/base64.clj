(ns clojure_exercises.base64
  (:use [clojure.test :only [deftest is run-tests]]))

(def ascii
  {\space 32 \! 33 \" 34 \# 35 \$ 36 \% 37 \& 38 \' 39 \( 40 \) 41 \* 42 \+ 43 \, 44 \- 45 \. 46 \/ 47 \0 48 \1 49 \2 50 \3 51 \4 52 \5 53 \6 54 \7 55 \8 56 \9 57 \: 58 \; 59 \< 60 \= 61 \> 62 \? 63 \@ 64 \A 65 \B 66 \C 67 \D 68 \E 69 \F 70 \G 71 \H 72 \I 73 \J 74 \K 75 \L 76 \M 77 \N 78 \O 79 \P 80 \Q 81 \R 82 \S 83 \T 84 \U 85 \V 86 \W 87 \X 88 \Y 89 \Z 90 \[ 91 \\ 92 \] 93 \^ 94 \_ 95 \` 96 \a 97 \b 98 \c 99 \d 100 \e 101 \f 102 \g 103 \h 104 \i 105 \j 106 \k 107 \l 108 \m 109 \n 110 \o 111 \p 112 \q 113 \r 114 \s 115 \t 116 \u 117 \v 118 \w 119 \x 120 \y 121 \z 122 \{ 123 \| 124 \} 125 \~ 126})
(def ascii (apply conj (map (fn [[k v]] {(.toString k) v}) ascii)))

(def index-table
  {0 A 16 Q 32 g 48 w 1 B 17 R 33 h 49 x 2 C 18 S 34 i 50 y 3 D 19 T 35 j 51 z 4 E 20 U 36 k 52 0 5 F 21 V 37 l 53 1 6 G 22 W 38 m 54 2 7 H 23 X 39 n 55 3 8 I 24 Y 40 o 56 4 9 J 25 Z 41 p 57 5 10 K 26 a 42 q 58 6 11 L 27 b 43 r 59 7 12 M 28 c 44 s 60 8 13 N 29 d 45 t 61 9 14 O 30 e 46 u 62 + 15 P 31 f 47 v 63 /}

(defn encode [text]
  ;TODO: pad as string to prevent converting back and forth between String and Integer
  (let [ascii-8-bit (fn [s] (format "%08d" (Integer/parseInt (Integer/toString (ascii s) 2))))
        ascii-bytes (map #(ascii-8-bit %) (re-seq #"." text))]
    ascii-bytes))
; disect into 4 pieces of 6 bits and get their code from the index-table

(deftest test-simple-text
    (is (= "TWFu" (encode "Man"))))

(run-tests)

