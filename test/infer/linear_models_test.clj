(ns infer.linear-models-test 
  (:use clojure.test)
  (:use infer.matrix)
  (:use infer.linear-models))

(def height
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
 1.83])

(def weight
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
 74.46])

(deftest simple-regresssion-tests
  (let [r1 (my-ols-linear-model 
	   weight height)
	r2 (ols-linear-model
	    weight height)]
    (is (= [[37.713145382089586]] (from-matrix r1)))	
    (is (= [37.713145382089586] (betas r2)))))