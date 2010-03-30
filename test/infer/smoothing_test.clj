(ns infer.smoothing-test
  (:use clojure.test
	infer.matrix
	infer.measures
	infer.smoothing))

(deftest all-are-true
  (is (not (all? [false])))
  (is (all? [true])))

;;TODO: make this stuff match matrix based apis.
(deftest find-points-for-query-point
  (is (=  [[2 0.7]
	   [3 0.3]
	   [4 0.3]]
	  (within-region (euclid-query 3 1)
	   [[1 0.5]
	    [2 0.7]
	    [3 0.3]
	    [4 0.3]
	    [5 0.8]])))
  (is (=   [[[3 2] 0.3]]
	   (within-region (rectangle-query 
	    [5 3] [2 1])
	    [[[1 3] 0.5]
	     [[2 7] 0.7]
	     [[3 2] 0.3]
	     [[4 6] 0.3]
	     [[5 1] 0.8]]))))

(deftest euclid-query-test
  (is (= [[[5 1] 0.8]]
	  (within-region (euclid-query
	   [5 3] 2)
	    [[[1 3] 0.5]
	     [[2 7] 0.7]
	     [[3 2] 0.3]
	     [[4 6] 0.3]
	     [[5 1] 0.8]]))))

(deftest nearest-neighbor-test
  (is (= [[[5 1] 0.8]
	  [[3 2] 0.3]]
	  (nearest-neighbors 2
	   [5 3] euclidean-distance
	    [[[1 3] 0.5]
	     [[2 7] 0.7]
	     [[3 2] 0.3]
	     [[4 6] 0.3]
	     [[5 1] 0.8]]))))

(deftest knn-test
  (is (= (/ 2.6 5) 
	 (mean-output
	  [[[1 3] 0.5]
	   [[2 7] 0.7]
	   [[3 2] 0.3]
	   [[4 6] 0.3]
	   [[5 1] 0.8]])))
  (is (= [(/ 2.6 5)
	  0.2]
	 (mean-output
	  [[[1 3] [0.5 0.2]]
	   [[2 7] [0.7 0.1]]
	   [[3 2] [0.3 0.4]]
	   [[4 6] [0.3 0.1]]
	   [[5 1] [0.8 0.2]]]))))

(deftest calc-weights
  (is (= [0.25 0.14285714285714285 0.3333333333333333
	  0.25 0.5]
	 (weights
	  [5 3]
	  (comp inverse manhattan-distance)
	  [[[1 3] 0.5]
	   [[2 7] 0.7]
	   [[3 2] 0.3]
	   [[4 6] 0.3]
	   [[5 1] 0.8]]))))

(deftest weighted-knn-test
  (is (= 0.5419354838709678
	 (nadaraya-watson-estimator
	  [5 3]
	  (comp inverse manhattan-distance)
	  [[[1 3] 0.5]
	   [[2 7] 0.7]
	   [[3 2] 0.3]
	   [[4 6] 0.3]
	   [[5 1] 0.8]])))
  (is (= [0.5419354838709678
	  0.5419354838709678]
	 (nadaraya-watson-estimator
	  [5 3]
	  (comp inverse manhattan-distance)
	  [[[1 3] [0.5 0.5]]
	   [[2 7] [0.7 0.7]]
	   [[3 2] [0.3 0.3]]
	   [[4 6] [0.3 0.3]]
	   [[5 1] [0.8 0.8]]]))))

(deftest uniform-test
  (is (= 0 (uniform 7)))
  (is (= 1/2 (uniform 0.764))))

(deftest triangular-test
  (is (= 0 (triangular 7)))
  (is (= 0.236 (triangular 0.764))))

(deftest epanechnikov-test
  (is (= 0 (epanechnikov 7)))
  (is (= 0.7103250000000001 (epanechnikov 0.23))))

(deftest biweight-test
  (is (= 0 (biweight 7)))
  (is (= 0.723538090666667 (biweight 0.42))))

(deftest triweight-test
  (is (= 0 (triweight 7)))
  (is (= 0.61103639653 (triweight 0.42))))

(deftest gaussian-test
  (is (= 0.05399096651318806 (gaussian 2)))
  (is (= 0.3652626726221539 (gaussian 0.42))))

(deftest cosine-test
  (is (= 0 (cosine 2)))
  (is (= 0.6205862955191552 (cosine 0.42))))