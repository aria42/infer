(ns infer.matrix-test
  (:use clojure.test
	infer.matrix))

(deftest inc-at-test
  (let [A (fill 0 3 3)]
    (inc-at A 0 0)
    (is (= 1 (get-at A 0 0)))
    (inc-at A 2 0 0)
    (is (= 3 (get-at A 0 0)))))

(deftest create-matrix
  (let [m (matrix
	   [[1 2 3] [4 5 6]])]
	   (is (= 6 (get-at m 1 2)))))

