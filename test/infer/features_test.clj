(ns infer.features-test
  (:use clojure.test)
  (:use infer.features))

(deftest flatten-seqs-test
  (is (= [[1 2] [3 4]]
     (flatten-seqs [[[1 2]] [[[3 4]]]])))
  (is (= [[0 1 1 1] [0 1 2 1] [0 1 2 1] [0 1 2 1] [0 2 1 1]]
     (flatten-seqs
      [[[[[0 1 1 1]] [[0 1 2 1] [0 1 2 1] [0 1 2 1]]] [[[0 2 1 1]]]]]))))

(deftest remove-at-test
  (is (= [0 1 3]
	 (remove-at 2 [0 1 2 3])))
  (is (= [1 2 3]
	 (remove-at 0 [0 1 2 3])))
  (is (= [0 1 2]
	 (remove-at 3 [0 1 2 3])))) 

(deftest vec-but-last-test
  (is (= [1 2 3]
	 (vec-but-last [1 2 3 4]))))

(deftest count-when-test
  (let [data [[0 6 0 2 6]
	      [0 6 0 2 6]
	      [0 6 0 2 6]
	      [0 6 0 2 6]]]
    
  (is (= 0 
	 (count-when
	  (partial nth-is? 2 #(> % 0))
	  data)))
  (is (= 0 
	 (count-when
	  (fn [c] (not (some #(> % 0) c)))
	  data)))))

(deftest make-feature-vectors
  (is (=
       [[0 1 1]
	[0 1 2]
	[0 1 2]
	[0 1 2]
	[0 2 1]]
       (feature-vectors
	{0 {1 {1 1 2 3}
	    2 {1 1}}})))
  (is (=
       [[0 1 1]
	[0 1 2]
	[0 1 2]
	[0 2 1]
	[2 1 2]
	[2 1 2]]
       (feature-vectors
	{0 {1 {1 1 2 2}
	    2 {1 1}}
	 2 {1 {2 2}}})))
  (is (=
       [[0 1 1]
	[0 1 2]
	[0 1 2]
	[0 2 1]
	[0 0 1]
	[2 1 2]
	[2 1 2]]
       (feature-vectors
	{0 {1 {1 1 2 2}
	    2 {1 1}
	    :missing {1 1}}
	 2 {1 {2 2}}}))))