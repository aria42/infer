(ns infer.features-test
  (:use clojure.test)
  (:use infer.features))

(deftest form-eqiv-classes
  (is (= {1 0, 2 0, 3 0, 4 1, 5 1, 6 1}
       (equivalence-classes {0 #{1 2 3}, 1 #{4 5 6}}))))

(deftest merge-into-equivalence-classes-test
  (let [classes (equivalence-classes {0 #{1 2 3}, 1 #{4 5 6}})
  all-zero (equivalence-classes {0 #{1 2 3 4 5 6}})]
  (is (= {1 {0 3, 1 2}
    0 {0 25, 1 21}
    :missing {0 1}}
   (merge-equivalence-classes
      {1 classes}
    {1 {0 25},
     3 {1 21}
     4 {0 3},
     6 {1 2}
     :missing {0 1}})))
  (is (= {1 {0 5}
    0 {0 46}
    :missing {0 1}}
   (merge-equivalence-classes
      {1 classes
       2 all-zero}
    {1 {0 25},
     3 {1 21}
     4 {0 3},
     6 {1 2}
     :missing {0 1}})))
  (is (= {1 {1 {0 3, 1 2}}
      0 {0 {0 25, 1 21}}}
     (merge-equivalence-classes
        {1 classes
         2 classes}
      {1 {0 {0 25}},
       3 {1 {1 21}}
       4 {4 {0 3}},
       6 {6 {1 2}}})))))

(deftest heterogenious-depth-merge-classes
  (let [classes (equivalence-classes {0 #{1 2 3}, 1 #{4 5 6}})]
  (is (= {2 {1 {0 3, 1 2}}
      0 {0 46}
    :override 10}
     (merge-equivalence-classes
        {1 (equivalence-classes {:override #{0} 0 #{1 2}, 1 #{3}, 2 #{4 5 6 7}})
         2 classes}
      {0 10,
     1 {0 25},
       2 {1 21},
       4 {4 {0 3}},
       6 {6 {1 2}}})))))

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
	[0 :missing 1]
	[2 1 2]
	[2 1 2]]
       (feature-vectors
	{0 {1 {1 1 2 2}
	    2 {1 1}
	    :missing {1 1}}
	 2 {1 {2 2}}}))))