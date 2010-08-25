(ns infer.matrix-test
  (:use clojure.test
	infer.matrix))

(deftest leave-out-columns
  (is (= #{0 1 4}
     (leave-out [2 3] (range 0 5)))))

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

(deftest to-and-from-sparse-matrix
  (let [a [{0 1, 5 2, 9 3} {4 4,9 5, 16 6}]
	A (sparse-matrix a)
	b (from-sparse-2d-matrix A)]
    (is (= a b))))

(deftest to-and-from-sparse-colt-matrix
  (let [a [{0 1, 5 2, 9 3} {4 4,9 5, 16 6}]
	A (sparse-colt-matrix a)
	b (from-sparse-2d-matrix A)]
    (is (= a b))))

(deftest to-and-from-sparse-pcolt-matrix
  (let [a [{0 1, 5 2, 9 3} {4 4,9 5, 16 6}]
	A (sparse-pcolt-matrix a)
	b (from-sparse-2d-matrix A)]
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

(deftest matrix-multiplication
  (let [A (matrix [[1 1] [1 1]])
	B (matrix [[2 2] [2 2]])
	C (matrix [[2 0] [0 2]])]
    (is (= (matrix [[4 4] [4 4]]) (times A B)))
    (is (= (matrix [[8 8] [8 8]]) (times A B C)))))

;; (deftest matrix-divide
;;   (let [A (matrix [[1 1] [1 1]])
;; 	B (matrix [[2 2] [2 2]])
;; 	C (matrix [[2 0] [0 2]])]
;;     (is (= (matrix [[0.5 0.5] [0.5 0.5]]) (divide A B)))
;;     (is (= (matrix [[8 8] [8 8]]) (divide A B C)))))

(deftest matrix-addition
  (let [A (matrix [[1 1] [1 1]])
	B (matrix [[2 2] [2 2]])
	C (matrix [[2 0] [0 2]])]
  (is (= (matrix [[3 3] [3 3]]) (plus A B)))  
  (is (= (matrix [[5 3] [3 5]]) (plus A B C)))))

(deftest matrix-subtraction
  (let [A (matrix [[1 1] [1 1]])
	B (matrix [[2 2] [2 2]])
	C (matrix [[2 0] [0 2]])]
  (is (= (matrix [[1 1] [1 1]]) (minus B A)))  
  (is (= (matrix [[-1 1] [1 -1]]) (minus B A C)))))

(deftest concat-columns
  (is (= (from-matrix
	  (matrix [[1 2 3]
		   [2 3 4]]))
	 (from-matrix (column-concat
		       (column-matrix [1 2])
		       (column-matrix [2 3])
		       (column-matrix [3 4]))))))