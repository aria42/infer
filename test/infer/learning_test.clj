(ns infer.features-test
  (:use clojure.test)
  (:use infer.matrix)
  (:use infer.core)
  (:use infer.learning))

(deftest feature-target-mutual-information
  (is (= [0.07290559532005605 0.07290559532005605
	  0.07290559532005605 0.07290559532005605]
	 (feature-target-mi (I 5 5)))))

(def test-features
	  [[0 1 0 0] 
	   [0 2 0 0] 
	   [0 3 0 0] 
	   [0 4 0 0] 
	   [0 5 0 0] 
	   [1 6 1 1] 
	   [1 7 1 1] 
	   [1 8 1 1] 
	   [1 9 1 1] 
	   [1 10 1 1]])

(deftest create-feature-mi-matrix
  (is (= [[0.9999999999999999 1.0 1.0]
	  [0.9999999999999999 0.9999999999999999 0.9999999999999999]
	  [1.0 0.9999999999999999 1.0]
	  [1.0 0.9999999999999999 1.0]]
	 (feature-mi-matrix
	  (matrix test-features)))))

(deftest find-max-index
  (is (= 2
	 (index-of-max [0 1 2] [0 1 2])))
  (is (= nil
	 (index-of-max [] [])))
  (is (= 6
	 (index-of-max [0 2 1] [0 6 7]))))

(deftest select-mrmr-subset
  (is (= [2 1]
	 (mrmr-feature-set
	  2 3
	  test-features))))

(defn f [x]
  (+ (- (pow x 4) (* 3 (pow x 3))) 2))

(defn within [x delta]
  #(not (or (< x (- % delta))
	    (> x (+ % delta)))))

(deftest gradient-decent-test
  (let [x 6
	step 0.01
	precision 0.00001]
    (is ((within 2.24996 0.00001)
	   (gradient-descent f step precision x)))))