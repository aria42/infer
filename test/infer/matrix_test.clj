(ns infer.matrix-test
  (:use clojure.test
	infer.matrix))

(deftest inc-at-test
  (let [A (fill 0 3 3)]
    (inc-at A 0 0)
    (is (= 1 (get-at A 0 0)))
    (inc-at A 2 0 0)
    (is (= 3 (get-at A 0 0)))))

(deftest ensure-vecs-test
  (let [v (ensure-vecs [[1]])
	s (ensure-vecs (list (list 1) (list 2)))
	vs (ensure-vecs [(list 1) (list 2)])]
  (is (vector? v))
  (is (vector? (first v)))
  (is (vector? s))
  (is (vector? (second s)))))

(deftest create-matrix
  (let [m (matrix
	   [[1 2 3] [4 5 6]])
	single-m (column-matrix [1 2 3 4 5 6])]
	   (is (= 6 (get-at m 1 2)))
	   (is (= 6 (get-at single-m 5 0)))))

(deftest to-and-from-matrix
  (let [a [[1 2 3] [4 5 6]]
	A (matrix
	   [[1 2 3] [4 5 6]])
	b (from-matrix A)]
	(is (= a b))))

(deftest to-and-from-column-matrix
  (let [a [1 2 3]
	A (column-matrix
	   a)
	b (from-column-matrix A)]
	(is (= a b))))

(deftest identity-matrix
  (let [i (from-matrix (I 2 2))]
	(is (= [[1 0]
		[0 1]]
	       i))))

(deftest create-diagonal-weights
  (is (= [[1 0 0]
	  [0 2 0]
	  [0 0 3]]
	  (to-diag [1 2 3]))))

(deftest concat-columns
  (is (= (from-matrix
	  (matrix [[1 2 3]
		   [2 3 4]]))
	 (from-matrix (column-concat
		       (column-matrix [1 2])
		       (column-matrix [2 3])
		       (column-matrix [3 4]))))))