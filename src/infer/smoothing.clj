(ns infer.smoothing
  (:use infer.measures)
  (:use infer.core)
  (:use infer.features)
  (:use clojure.contrib.math))

;;TODO: is motthing really the right name for this lib?  Density estimation?  k-NN & kernels?
;;TODO: change sigs to match the matrix apis of xs & ys rather that [xs & ys]


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
    #(<= (euclidean-distance point %)
	 dist))

;;TODO: point-space queries don't include distance in the y dimension.
(defn rectangle-query [point lengths]
"Like manhattan distance but different in that rather than specifying a query for points as a region defined by a distance as a radius in some metric space, it specifies a regious as a hyper-rectangle by leting a vector of lenghts represent the bounds around the point."
  (fn [xys]
    (all? (map (fn [x r p]
		 (<= (one-d-distance p x) r))
	       xys
	       lengths
	       point))))

;;nearest neighbors fitlers points for k-nn, within-region filters points based a region query; a hyper rectangle or a radius for kernels.

;;http://en.wikipedia.org/wiki/K-nearest_neighbor_algorithm
;;http://en.wikipedia.org/wiki/Nearest_neighbor_search
;;naive impl
(defn nearest-neighbors [k point dist coll]
  (take k
	(sort-by 
	 #(dist point (vec-but-last %))
	 coll)))

(defn within-region [pred coll]
  (filter (comp pred vec-but-last) coll))

;;TODO: create an explict format for xs vs. ys in feature vecs?  linear models split xs & ys into different inputs, if they are in the same feature vectors, then how do we divide - especailly for multi-y or multi-class outputs.
;;here we assume that there is y.
(defn single-class? [points]
  (number? (last (first points))))

;;inverse is a common nn weighing function, but not technicly a kernel.
(defn inverse [d] 
  (if (= 0 d) Double/POSITIVE_INFINITY
      (/ 1 d)))

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
      (mean (map vec-last points))
      (map mean (seq-trans (map vec-last points)))))

(defn weights [point weigh points]
  (map #(weigh point (vec-but-last %)) points))

;;kernel regression
;;http://en.wikipedia.org/wiki/Kernel_regression

;;kernel smoother
;;http://en.wikipedia.org/wiki/Kernel_smoother

;;TODO:
;;1. pass the distance fn and weighing fn seperately rahter than composing into weigh prior to calling?
;;for kernels, but weighted mean calc is identical for k-nn
(defn nadaraya-watson-estimator [point weigh points]
"takes a query point, a weight fn, and a seq of points, and returns the weighted sum of the points divided but the sum of the weights. the weigh fn is called with the query point and each point in the points seq.  the weigh fn is thus a composition of a weight fn and a distance measure.

http://en.wikipedia.org/wiki/Kernel_regression#Nadaraya-Watson_kernel_regression

"
  (let [weights* (weights point weigh points)
	divisor (sum weights*)]
    (if (single-class? points)
	(/
	 (weighted-sum (map vec-last points) weights*)
	 divisor)
        (map #(/ % divisor)
	     (map #(weighted-sum % weights*)
		  (seq-trans (map vec-last points)))))))

;;TODO: other kernel estimators?
;;Priestley-Chao kernel estimator
;;Gasser-Müller kernel estimator
;;http://en.wikipedia.org/wiki/Kernel_regression

;;TODO: top level api exploration
(defn smoother [query weigh data]
  (fn [x]
    (nadaraya-watson-estimator
     x weigh (query x data))))

(defn knn-smoother [k data]
  (smoother #(nearest-neighbors k (vec-but-last %1)
				euclidean-distance %2)
	    (comp inverse euclidean-distance)
	    data))