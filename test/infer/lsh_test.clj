(ns infer.lsh-test
  (:use infer.lsh)
  (:use clojure.test))

; === A few math tests. === ;

(deftest jaccard-identical
	(is (= (jaccard #{1 2 3} #{1 2 3}) 1) "#{1 2 3} did not show up as equal to #{1 2 3}."))

(deftest jaccard-orthogonal
	(is (= (jaccard #{1 2 3} #{4 5 6}) 0) "#{1 2 3} did not show up as orthogonal to #{4 5 6}"))

(deftest jaccard-partial-overlap
	(is (= (jaccard #{1 2 3} #{2 3 4}) 0.5) "#{1 2 3} and #{2 3 4} should have a jaccard sim of .5."))

(deftest test-dot-product
  (is (= 100 (dot-product [5 5 5 5] [5 5 5 5]))))

; === Testing helper functions. === ;

; Might phase out this functionality soon.
(deftest test-interleave-and-partition
  (is (= [[1 10 100] [2 20 200] [3 30 300]] (interleave-and-partition [1 2 3] [10 20 30] [100 200 300])))
  ;(is (= [[1 1 1 1]] (interleave-and-partition [1 1 1 1])))
  ;(is (= [{} {}] (interleave-and-partition {0 0, 1 1} {0 0, 1 1})))
  )

; === Testing hashing functions. === ;

(deftest test-exact-minhash
  (is (=
       0
       ((exact-minhash {3 0, 2 1, 1 2, 0 3}) #{1 3})))
  (is (=
       [0 1]
       (map
	#(% #{3 2})
	(map
	 exact-minhash [{3 0, 2 1, 1 2, 0 3} {3 2, 2 1, 1 0, 0 3}])))))

(deftest test-hamming-hash
  (is (= 1
	 ((hamming-hash 3) [0 0 0 1 0 0 0 0 0])))
  (is (= [1 10]
	   (map #(% [1 2 10 4 5 6 7 8 9 20])
		(map hamming-hash [0 2])))))

; (deftest test-l1-hash
;   "Needs to be rewritten ... function is a bit sloppy, not entirely sure it's right."
;   (is (= 1 0) "Function not implemented."))  

(deftest test-lp-hash
  (is (= 1 ((lp-hash [1 1 1 1 1] 1 5) [1 1 1 1 1])))
  (is (= [1 0]
	   (map #(% [1 1 1 1 1])
		(map lp-hash
		     [[1 1 1 1 1] [1 1 0 0 0]]
		     [1  1]
		     [5 5])))))

; (deftest test-spherical-l2-hash
;   (is (= 1 0) "Function not implemented.")) 