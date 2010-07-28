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