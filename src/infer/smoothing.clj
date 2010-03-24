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

(defn range-query [point radius points]
  (let [one-d (number? point)
	range-pred (if one-d #(<= (abs (- point (first %))) radius)
		       (fn [xys]
			 (all? (map (fn [x r p]
				      (<= (abs (- p x)) r))
				    (first xys)
				    radius
				    point))))]
    (filter range-pred
	    points)))

(defn knn [points]
  (mean (map second points)))

(defn weighted-knn [point weigh points]
  (let [distance (fn [p] (sum (map #(abs (- %1 %2)) p point)))
	weights (map (comp weigh distance first) points)
	weighted (weighted-sum (map second points) weights)]
    (prn distance " " weights " " weighted)
    (/ weighted (sum weights))))