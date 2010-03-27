(ns infer.smoothing
  (:use infer.measures)
  (:use infer.features)
  (:use clojure.contrib.math))

;;TODO: is motthing really the right name for this lib?  Density estimation?  k-NN & kernels?
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

;;http://en.wikipedia.org/wiki/K-nearest_neighbor_algorithm
;;http://en.wikipedia.org/wiki/Nearest_neighbor_search
;;naive
(defn nearest-neighbors [k point dist coll]
  (take k
	(sort-by 
	 #(dist point (first %))
	 coll)))

;;TODO: change sigs to match the matrix apis of xs & ys rather that [xs & ys]
(defn within-region [pred coll]
  (filter (comp pred first) coll))

(defn single-class? [points]
  (number? (second (first points))))

;;TODO: move elsewhere with lib of kernel fns.
(defn inverse [d] (/ 1 d))

(defn mean-output [points]
    (if (single-class? points)
      (mean (map second points))
      (map mean (seq-trans (map second points)))))

(defn weighted-knn [point weigh points]
"takes a query point, a weight fn, and a seq of points, and returns the weighted sum of the points divided but the sum of the weights. the weigh fn is called with the query point and each point in the points seq.  the weigh fn is thus a composition of a weight fn and a distance measure.
"
  (let [weights (map #(weigh point (first %)) points)
	divisor (sum weights)]
    (if (single-class? points)
	(/
	 (weighted-sum (map second points) weights)
	 divisor)
        (map #(/ % divisor)
	     (map #(weighted-sum % weights)
		  (seq-trans (map second points)))))))