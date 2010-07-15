(ns infer.learning-test
  (:use clojure.test)
  (:use infer.matrix)
  (:use infer.features)
  (:use infer.measures)
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

(deftest dydx-test
  (is (= [3] (dydx #(* 3 %) [2] [6]))))

(deftest gradient-decent-test
  (let [x 6
	step 0.01
	precision 0.00001]
    (is (within 0.00001 2.24996
	   (first (gradient-descent f step precision x))))))

;;optimize the derivative of the loss
(deftest sgd-test
  (let [x 3
	step 0.01
	precision 0.0000001
	examples (repeatedly #(let [x (+ 1 (rand-int 9))] [x  (* 6 x)]))]
    (is (within 0.001 2
	   (first (sgd (fn [[x1] [x0]] (* 3 x1 x0))
		       (fn [f x1 e1] (map #(* 2 (- (f x1 (vec-but-last e1))
						(vec-last e1))
					%) x1))
		       step precision [x] examples))))))

;;optimize the instantanious gradient of the loss.
;; (deftest sgd2-test
;;   (let [x 3
;; 	step 0.01
;; 	precision 0.0000001
;; 	examples (repeatedly #(let [x (+ 1 (rand-int 9))] [x  (* 6 x)]))]
;;     (is (within 0.001 2
;; 	   (first (sgd2 (fn [[x1] [x0]] (* 3 x1 x0))
;; 		       (fn [x1 x0] (pow (- x1 x0) 2))
;; 		       step precision [x] examples))))))

(deftest active-set-test
  (is (= [0 2]
	 (active-set [2 0 1]))))
