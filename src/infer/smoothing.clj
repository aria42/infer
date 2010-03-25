(ns infer.smoothing
  (:use infer.measures)
  (:use clojure.contrib.math))

;;smoothing.
;;http://en.wikipedia.org/wiki/Smoothing
;;http://en.wikipedia.org/wiki/Kernel_smoother
;;http://en.wikipedia.org/wiki/Kernel_density_estimation
;;http://en.wikipedia.org/wiki/Local_regression
;;http://en.wikipedia.org/wiki/Kernel_regression

(defn all? [bools]
  (every? identity bools))

(defn one-d-distance [x1 x2]
 (abs (- x1 x2)))

(defn euclid-query [point dist] 
  (if (number? point)
    #(<= (one-d-distance point %) dist)
    #(<= (euclidean-distance point %)
	 dist)))

(defn rectangle-query [point lengths]
  (fn [xys]
    (all? (map (fn [x r p]
		 (<= (one-d-distance p x) r))
	       xys
	       lengths
	       point))))

(defn filter-by [keyfn pred coll]
  (filter (comp pred keyfn) coll))

(defn filter-xys [pred coll]
  (filter (comp pred first) coll))

(defn knn [points]
  (mean (map second points)))

(defn inverse [d] (/ 1 d))

(defn weighted-knn [point weigh points]
"takes a query point, a weight fn, and a seq of points, and returns the weighted sum of the points divided but the sum of the weights. the weigh fn is called with the query point and each point in the points seq.  the weigh fn is thus a composition of a weight fn and a distance measure.
"
  (let [weights (map #(weigh point (first %)) points)
	weighted (weighted-sum (map second points) weights)]
    (/ weighted (sum weights))))

;;TODO: parameterize distance fn into weighing.