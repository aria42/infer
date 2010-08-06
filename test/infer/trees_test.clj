(ns infer.trees-test
  (:use infer.trees
	infer.probability
	clojure.test))

(deftest static-tree
  (let [n-l (leaf 1 (ge 10) 1 2)
	n-r (leaf 1 (ge 0) 3 4)
	t (node 0 (ge 5) n-l n-r)]
    (is (= 3 (t [4 6])))))

;;TODO:
;;continuous vs. discrete features
;;missing values
;;optimal stopping / pruning

(deftest midpoints-test
  (let [points [1 2 3 4 5]]
    (is (= [1.5 2.5 3.5 4.5]
	   (midpoints points)))))

(deftest count-splits
  (is (= [2 1]
	 (count-split [[2][3][4]] 0 3))))

(deftest count-withins
  (is (= 2
	 (count-within [[2][3][4][5][6]] 0 3 5))))


(deftest best-splits
  (is (= [[0 4 6/5]
	  [1 5 6/5]
	  [2 7 6/5]]
	 (best-split [[2 2 4]
		      [3 4 6]
		      [4 5 7]
		      [5 7 9]
		      [6 9 9]]))))
