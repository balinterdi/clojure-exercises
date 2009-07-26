(ns vars-bindings-namespaces
  (:use [clojure.contrib.str-utils :only (re-split str-join)]))

(defn ellipsize [words]
  (let [[w1 w2 w3] (re-split #"\s+" words)]
    (str-join " " [w1 w2 w3 "..."])))
    
(defn greet []
  "hello")

