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

(deftest simple-regression-tests
  (let [r1 (ols-linear-model 
	   weight height)
	r2 (commons-ols-linear-model
	    weight height)]
    (is (= [[37.713145382089586]] (from-matrix r1)))
    (is (= [[60.34103261134334]] (from-matrix (predict r1 [1.6]))))
    (is (= [37.713145382089586] (commons-betas r2)))))

(deftest regression-with-two-ys
  (let [r1 (ols-linear-model 
	   (partition 2 (interleave weight weight))
	   height)
	r2 (ols-linear-model 
	   (partition 2 (interleave weight (map #(* 2 %) weight)))
	   height)]
    (is (= [[37.713145382089586 37.713145382089586]] (from-matrix r1)))
    (is (= [[60.34103261134334] [60.34103261134334]] (from-matrix (predict r1 [1.6]))))
    (is (= [[37.713145382089586 (* 2 37.713145382089586)]] (from-matrix r2)))))

(deftest gls-regression-tests
  (let [n (count weight)
	bs (gls-linear-model
	    weight height (I n n))
	bs2 (commons-gls-linear-model
	    weight height (to-diag (repeat n 1)))]
    (is (= [[37.713145382089586]] (from-matrix bs)))
    (is (= [37.713145382089586] (commons-betas bs2)))))

(deftest weighted-regression-tests
  (let [n (count weight)
	bs (weighted-linear-model
	    weight height (repeat n 1))
	bs2 (weighted-linear-model
	    weight height (range 1 (+ 1 n)))
	bs3 (commons-gls-linear-model
	    weight height (to-diag (range 1 (+ 1 n))))]
    (is (= [[37.713145382089586]] (from-matrix bs)))
    (is (= [[36.56120274286958]] (from-matrix bs2)))
    (is (= [36.561202742869575] (commons-betas bs3)))))

(deftest ridge-regression-tests
  (let [n (count weight)
	bs2 (ridge-regression
	    weight height 2)]
    (is (= [[35.96121310378788]] (from-matrix bs2)))))