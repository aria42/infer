(ns infer.matrix-factorization-test
  (:use clojure.test
        [infer.matrix :only [matrix I times]]
	      infer.matrix-factorization))

(defn- near-zero [x]
  (< (Math/abs x) 1.0e-8))

(deftest frobenius-error-test
  (let [A (matrix [[1 1] [1 1]])
	      B (matrix [[2 2] [2 2]])
	      C (times A B)]
	  (is (near-zero (frobenius-error C A B)))))
	 
(defn decreasing-seq? [xs]
  (every? 
    (fn [[x y]] (> x y))
    (partition 2 1 xs)))	 
	 
(deftest lee-seung-factorization-test
  (let [M (I 50 50) k 20
        err-seq 
          (for [iters [1 10 100]]
            (let [[X Y] (lee-seung-factorize M k {:num-iters iters})]
              (frobenius-error M X Y)))]
    (is (decreasing-seq? err-seq))))
