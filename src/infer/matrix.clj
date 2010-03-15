(ns infer.matrix
	(:refer-clojure :exclude (inc))
	(:use incanter.core))
	
(defn inc
  ([m x y] (inc m x y 1))
  ([m x y amount]
     (let [m2 (matrix m)]
       (.set m2 x y (+ (.getQuick m x y) amount))
       m2)))
		

