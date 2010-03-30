(ns infer.smoothing
  (:use infer.measures)
  (:use infer.core)
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
"Like manhattan distance but different in that rather than specifying a query for points as a region defined by a distance as a radius in some metric space, it specifies a regious as a hyper-rectangle by leting a vector of lenghts represent the bounds around the point."
  (fn [xys]
    (all? (map (fn [x r p]
		 (<= (one-d-distance p x) r))
	       xys
	       lengths
	       point))))

;;http://en.wikipedia.org/wiki/K-nearest_neighbor_algorithm
;;http://en.wikipedia.org/wiki/Nearest_neighbor_search
;;naive impl
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

;;TODO: move elsewhere with lib of kernel fns?  inverse is a common nn weighing function, but not technicly a kernel.
(defn inverse [d] (/ 1 d))

;;Kernel weighing functions
;;http://en.wikipedia.org/wiki/Kernel_(statistics)
(defn uniform [u]
  (if (> (abs u) 1) 0
  1/2))

(defn triangular [u]
 (if (> (abs u) 1) 0
 (- 1 (abs u))))

(defn epanechnikov [u]
  (if (> (abs u) 1) 0
  (* 3/4 (- 1 (pow u 2)))))

(defn biweight [u]
  (if (> (abs u) 1) 0
  (* 16/15 (pow (- 1 (pow u 2)) 2))))

(defn triweight [u]
  (if (> (abs u) 1) 0
  (* 35/32 (pow (- 1 (pow u 2)) 3))))

(defn gaussian [u]
  (*
   (/ 1 (sqrt (* 2 pi)))
   (pow e (* -1/2 (pow u 2)))))

(defn cosine [u]
  (if (> (abs u) 1) 0
      (*
       (/ pi 4)
       (cos (* (/ pi 2) u)))))

(defn mean-output [points]
    (if (single-class? points)
      (mean (map second points))
      (map mean (seq-trans (map second points)))))

;;kernel regression
;;http://en.wikipedia.org/wiki/Kernel_regression

;;kernel smoother
;;http://en.wikipedia.org/wiki/Kernel_smoother

;;TODO:
;;1. pass the distance fn and weighing fn seperately rahter than composing into weigh prior to calling?
(defn nadaraya-watson-estimator [point weigh points]
"takes a query point, a weight fn, and a seq of points, and returns the weighted sum of the points divided but the sum of the weights. the weigh fn is called with the query point and each point in the points seq.  the weigh fn is thus a composition of a weight fn and a distance measure.

http://en.wikipedia.org/wiki/Kernel_regression#Nadaraya-Watson_kernel_regression

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

;;TODO: other kernel estimators?
;;Priestley-Chao kernel estimator
;;Gasser-Müller kernel estimator
;;http://en.wikipedia.org/wiki/Kernel_regression