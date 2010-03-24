(ns infer.smoothing-test
  (:use clojure.test
	infer.matrix
	infer.smoothing))

(deftest all-are-true
  (is (not (all? [false])))
  (is (all? [true])))

(deftest find-points-for-query-point
  (is (=  [[2 0.7]
	   [3 0.3]
	   [4 0.3]]
	  (range-query
	   3 1
	   [[1 0.5]
	    [2 0.7]
	    [3 0.3]
	    [4 0.3]
	    [5 0.8]])))
  (is (=   [[[3 2] 0.3]]
	   (range-query
	    [5 3] [2 1]
	    [[[1 3] 0.5]
	     [[2 7] 0.7]
	     [[3 2] 0.3]
	     [[4 6] 0.3]
	     [[5 1] 0.8]]))))

(deftest knn-test
  (is (= (/ 2.6 5) 
       (knn
	[[[1 3] 0.5]
	     [[2 7] 0.7]
	     [[3 2] 0.3]
	     [[4 6] 0.3]
	     [[5 1] 0.8]]))))

(deftest weighted-knn-test
  (is (= 0.5419354838709678
	 (weighted-knn
	  [5 3]
	  (fn [d] (/ 1 d))
	  [[[1 3] 0.5]
	   [[2 7] 0.7]
	   [[3 2] 0.3]
	   [[4 6] 0.3]
	   [[5 1] 0.8]]))))
