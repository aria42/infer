(ns infer.lsh-test
  (:use infer.lsh)
  (:use clojure.test))

(deftest test-dot-product
  (is (= 100 (dot-product [5 5 5 5] [5 5 5 5]))))
  
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