(ns infer.matrix-test
  (:use clojure.test
		incanter.core)
  (:require (infer [matrix :as m])))

(deftest inc-test
	(let [A (matrix 0 3 3)]
		(is (= (matrix [[1 0 0][0 0 0][0 0 0]])
			   (m/inc A 0 0)))
		(is (= (matrix [[2 0 0][0 0 0][0 0 0]])
			   (m/inc A 0 0 2)))))