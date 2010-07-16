(ns infer.linear-models-test 
  (:use clojure.test)
  (:require [infer.io :as io])
  (:use infer.matrix)
  (:use infer.measures)
  (:use infer.linear-models))

(def height
(column-matrix
[1.47
 1.5
 1.52
 1.55
 1.57
 1.6
 1.63
 1.65
 1.68
 1.7
 1.73
 1.75
 1.78
 1.8
 1.83]))

(def weight
(column-matrix
[52.21
 53.12
 54.48
 55.84
 57.2
 58.57
 59.93
 61.29
 63.11
 64.47
 66.28
 68.1
 69.92
 72.19
 74.46]))

(deftest simple-regression-tests
  (let [r1 (ols-linear-model 
	   weight height)]
    (is (= [[37.71314538208958]] (from-matrix r1)))
    (is (= [[60.34103261134333]] (from-matrix
				  (predict r1 (matrix [[1.6]])))))))

(deftest regression-with-two-ys
  (let [weight-seq (from-column-matrix weight)
	r1 (ols-linear-model 
	   (matrix (partition 2
			      (interleave weight-seq weight-seq)))
	   height)
	r2 (ols-linear-model 
	   (matrix (partition 2
			      (interleave weight-seq
					  (map #(* 2 %) weight-seq))))
	   height)]
    (is (= [[37.71314538208958 37.71314538208958]] (from-matrix r1)))
    (is (= [[60.34103261134333] [60.34103261134333]]
	   (from-matrix (predict r1 (matrix [[1.6]])))))
    (is (= [[37.71314538208958 (* 2 37.71314538208958)]] (from-matrix r2)))))

(deftest gls-regression-tests
  (let [n (row-count weight)
	bs (gls-linear-model
	    weight height (I n n))]
    (is (= [[37.71314538208958]] (from-matrix bs)))))

(deftest weighted-regression-tests
  (let [n (row-count weight)
	bs (weighted-linear-model
	    weight height (repeat n 1))
	bs2 (weighted-linear-model
	    weight height (range 1 (+ 1 n)))]
    (is (= [[37.71314538208958]] (from-matrix bs)))
    (is (= [[36.56120274286953]] (from-matrix bs2)))))

(deftest ridge-regression-tests
  (let [n (row-count weight)
	bs2 (ridge-regression
	    weight height 2)]
    (is (= [[35.96121310378787]] (from-matrix bs2)))))

(def sp (column-matrix [2 0 0 0 1 1 0 0 1 0 0 1 0 0 1 0 1 1 1 1 1 0 2 1 1 2 2 2 2 0 1 0 0 2 0 2 1 2 0 1 2 0 0 1 0 0 2 0 0 2 2 2 1 1 2 2 0 2 1 2 2 1 1 2 0 1 1 2 2 0 0 1 1 2 2 1 1 0 1 1 0 1 1 0 0 0 1 0 1 0 0 1 1 0 1 0 1 0 0 2 0 1 0 0 0 0 0 1 0 0 0 2 1 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 1 0 0 1 1 1 0 0 0 1 0 0 0 0 0 0 0 1 0 1 0 1 0 0 0 1 2 0 0 0 0 0 0 1 1 0 0 0 2 0 2 0 1 0 1 1 1 0 2 0 0 2 0 1 0 0 0 0 1 2 0 0 0 1 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 1 0 1 1 1 0 0 0 0 0 0 0 0 2 0 0 0 2 0 0 0 0 2 1 0 1 1 1 0 0 1 1]))

(def in (column-matrix [1 1 2 2 1 2 0 0 0 0 1 2 1 2 1 2 2 0 2 0 0 2 0 0 1 0 0 0 1 2 0 1 1 0 1 0 0 1 0 0 0 0 0 1 0 1 0 2 2 0 1 1 1 0 0 0 1 0 0 0 0 0 0 0 1 0 0 1 0 1 1 0 0 0 0 0 0 1 0 0 2 0 0 2 0 2 0 2 1 0 2 0 0 0 1 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0 0 0 0 2 0 1 1 0 0 0 1 0 1 2 1 1 2 1 1 1 1 1 1 2 1 1 2 1 0 0 0 0 0 2 1 0 1 0 0 0 0 2 0 0 0 0 0 0 2 0 2 0 0 0 0 0 1 0 0 0 0 0 1 0 1 1 1 1 0 0 2 0 0 0 0 0 2 1 0 0 0 2 0 0 0 2 0 0 0 0 0 0 0 0 1 2 1 1 2 2 2 0 1 0 2 1 0 1 1 1 0 1 0 1 0 2 0 1 0 1 0 0 1 1 0 0 0 0 2 0 0]))

(def Y (column-matrix [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))

(def X (column-concat
	(column-matrix (repeat (count-elements sp) 1))
		sp
		in))

(def Bguess (column-matrix [0 0 0]))

(deftest irls-test
   (is (>= 0.0001 (euclidean-distance
	 [-1.7078 1.1972 0.4182]
 	 (from-column-matrix (irls Y X logistic-link binomial-variance Bguess 0.0001))))))

;; (deftest lasso-test
;;  (let [X (io/csv->matrix "/home/bradford/infer/data/lars_x.csv")
;;        Xintercept (column-concat (column-matrix (repeat (row-count X) 1)) X)
;; 	Y (io/csv->matrix "/home/bradford/infer/data/lars_y.csv")
;; 	B (io/csv->matrix "/home/bradford/infer/data/lars_betas.csv")
;; 	lambda (get-at (io/csv->matrix "/home/bradford/infer/data/lars_lambda.csv")
;; 		       0 0)
;; 	precision 0.00001
;;         Bold (column-matrix (repeat (+ 1 (column-count X)) 0))
;; 	ourB (lasso Y Xintercept Bold lambda precision)]
;;  (is (= B ourB))))
	