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

(deftest weighted-knn-test
  (is (= 0.5419354838709678
	 (weighted-knn
	  [5 3]
	  (comp inverse manhattan-distance)
	  [[[1 3] 0.5]
	   [[2 7] 0.7]
	   [[3 2] 0.3]
	   [[4 6] 0.3]
	   [[5 1] 0.8]])))
  (is (= [0.5419354838709678
	  0.5419354838709678]
	 (weighted-knn
	  [5 3]
	  (comp inverse manhattan-distance)
	  [[[1 3] [0.5 0.5]]
	   [[2 7] [0.7 0.7]]
	   [[3 2] [0.3 0.3]]
	   [[4 6] [0.3 0.3]]
	   [[5 1] [0.8 0.8]]]))))