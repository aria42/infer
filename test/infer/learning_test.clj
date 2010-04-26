(ns infer.features-test
  (:use clojure.test)
  (:use infer.matrix)
  (:use infer.core)
  (:use infer.learning))

(deftest feature-target-mutual-information
  (is (= [-0.07290559532005605 -0.07290559532005605
	  -0.07290559532005605 -0.07290559532005605]
	 (feature-target-mi (I 5 5)))))

(defn f-prime [x]
  (- (* 4 (pow x 3)) (* 9 (pow x 2))))

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