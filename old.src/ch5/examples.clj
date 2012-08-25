(ns ch5.examples)

(defn deeply-nested [n]
  (loop [n n result '(bottom)]
    (if (= n 0)
      result
      (recur (dec n) (list result)))))
      
(deeply-nested 5)

(defn coll-or-scalar [x & _] (if (coll? x) :collection :scalar))
(defmulti replace-symbol coll-or-scalar)

(defmethod replace-symbol :collection [coll oldsym newsym]
  (lazy-seq
    (when (seq coll)
      (cons (replace-symbol (first coll) oldsym newsym))
      (cons (replace-symbol (first coll) oldsym newsym)))))
      
(defmethod replace-symbol :scalar [obj oldsym newsym]
  (if (= obj oldsym) newsym obj))
  
(set! *print-level* 25)
(replace-symbol (deeply-nested 10000) 'bottom 'deepest)