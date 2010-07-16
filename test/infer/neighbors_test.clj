(ns infer.neighbors-test
  (:use clojure.test
	infer.matrix
	infer.measures
	infer.neighbors))

(deftest all-are-true
  (is (not (all? [false])))
  (is (all? [true])))

;;TODO: make this stuff match matrix based apis.
(deftest find-points-for-query-point
  (is (=  [[2 0.7]
	   [3 0.3]
	   [4 0.3]]
	  (within-region (euclid-query [3] 1)
	   [[1 0.5]
	    [2 0.7]
	    [3 0.3]
	    [4 0.3]
	    [5 0.8]])))
  (is (=   [[3 2 0.3]]
	   (within-region (rectangle-query 
	    [5 3] [2 1])
	    [[1 3 0.5]
	     [2 7 0.7]
	     [3 2 0.3]
	     [4 6 0.3]
	     [5 1 0.8]]))))

(deftest euclid-query-test
  (is (= [[5 1 0.8]]
	  (within-region (euclid-query
	   [5 3] 2)
	    [[1 3 0.5]
	     [2 7 0.7]
	     [3 2 0.3]
	     [4 6 0.3]
	     [5 1 0.8]]))))

(deftest nearest-neighbor-test
  (is (= [[5 1 0.8]
	  [3 2 0.3]]
	  (nearest-neighbors 2
	   [5 3] euclidean-distance
	    [[1 3 0.5]
	     [2 7 0.7]
	     [3 2 0.3]
	     [4 6 0.3]
	     [5 1 0.8]]))))

(deftest knn-test
  (is (= (/ 2.6 5) 
	 (mean-output
	  [[1 3 0.5]
	   [2 7 0.7]
	   [3 2 0.3]
	   [4 6 0.3]
	   [5 1 0.8]])))
  (is (= [(/ 2.6 5)
	  0.2]
	 (mean-output
	  [[1 3 [0.5 0.2]]
	   [2 7 [0.7 0.1]]
	   [3 2 [0.3 0.4]]
	   [4 6 [0.3 0.1]]
	   [5 1 [0.8 0.2]]]))))

(deftest calc-weights
  (is (= [0.25 0.14285714285714285 0.3333333333333333
	  0.25 0.5]
	 (weights
	  [5 3]
	  (comp inverse manhattan-distance)
	  [[1 3 0.5]
	   [2 7 0.7]
	   [3 2 0.3]
	   [4 6 0.3]
	   [5 1 0.8]]))))

(deftest weighted-knn-test
  (is (= 0.5419354838709678
	 (nadaraya-watson-estimator
	  [5 3]
	  (comp inverse manhattan-distance)
	  [[1 3 0.5]
	   [2 7 0.7]
	   [3 2 0.3]
	   [4 6 0.3]
	   [5 1 0.8]])))
  (is (= [0.5419354838709678
	  0.5419354838709678]
	 (nadaraya-watson-estimator
	  [5 3]
	  (comp inverse manhattan-distance)
	  [[1 3 [0.5 0.5]]
	   [2 7 [0.7 0.7]]
	   [3 2 [0.3 0.3]]
	   [4 6 [0.3 0.3]]
	   [5 1 [0.8 0.8]]]))))

(deftest uniform-test
  (is (= 0 (uniform 7)))
  (is (= 1/2 (uniform 0.764))))

(deftest triangular-test
  (is (= 0 (triangular 7)))
  (is (= 0.236 (triangular 0.764))))

(deftest epanechnikov-test
  (is (= 0 (epanechnikov 7)))
  (is (= 0.7103250000000001 (epanechnikov 0.23))))

(deftest biweight-test
  (is (= 0 (biweight 7)))
  (is (= 0.723538090666667 (biweight 0.42))))

(deftest triweight-test
  (is (= 0 (triweight 7)))
  (is (= 0.61103639653 (triweight 0.42))))

(deftest gaussian-test
  (is (= 0.05399096651318806 (gaussian 2)))
  (is (= 0.3652626726221539 (gaussian 0.42))))

(deftest cosine-test
  (is (= 0 (cosine 2)))
  (is (= 0.6205862955191552 (cosine 0.42))))

(deftest basic-exact-minhash
  (let [perm-dict-1 {3 0, 2 1, 1 2, 0 3}
	perm-dict-2 {3 2, 2 1, 1 0, 0 3}
	p1 #{1 3}
	p2 #{3 2}]
  (is (= 0 ((exact-minhash perm-dict-1) p1)))
  (is (= [0 1]
       (map
         #(% p2)
         (map
           exact-minhash 
           [perm-dict-1 
            perm-dict-2]))))))

(deftest basic-hamming-hash
  (let [v1 [0 0 0 1 0 0 0 0 0]
	v2 [1 2 10 4 5 6 7 8 9 20]] 
  (is (= 1 ((hamming-hash 3) v1)))
  (is (= [1 10]
    (map #(% v2)
         (map hamming-hash [0 2]))))))

(deftest basic-lp-hash
  (let [v1 [1 1 1 1 1] v2 [1 1 0 0 0]]
  (is (= 1 ((lp-hash v1 1 5) v1)))
  (is (= [1 0]
      (map #(% v1)
       (map lp-hash [v1 v2] [1  1] [5 5]))))))

(deftest add-to-tables
  (let [v1 [0 0 3 0 0 0 0 0 0]
	v2 [1 2 3 4 5 6 7 8 9 20]
	lsh-table [{} {}]
	hamming-hash-fcns [(hamming-hash 1) (hamming-hash 2)]
	hp1 (map #(% v1) hamming-hash-fcns)
	hp2 (map #(% v2) hamming-hash-fcns)]
    (is (= [{[2] #{1}} {[3] #{1}}]
	   (assoc-lsh lsh-table hp2 1)))
    (is (= [{[2] #{2}, [0] #{1}}
	    {[3] #{1, 2}}]
	     (assoc-lsh lsh-table hp1 1 hp2 2)
	     ))))

(deftest merging-multiple-tables
  (let [table1
	[{[1 1] #{1 2}, [2 2] #{3 4}}
	 {[1 2] #{1}, [2 3] #{2 3 4}}]
	table2
	[{[1 1] #{5}, [2 3] #{6 7}}
	 {[1 2] #{6}, [2 4] #{5 7}}]]
    (is (= [{[1 1] #{1 2 5}, [2 2] #{3 4}, [2 3] #{6 7}}
	    {[1 2] #{1 6}, [2 3] #{2 3 4}, [2 4] #{5 7}}]
	     (merge-tables table1 table2)))))