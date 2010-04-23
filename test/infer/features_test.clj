(ns infer.features-test
  (:use clojure.test)
  (:use infer.matrix)
  (:use infer.core)
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
	  data)))
  (is (= 4
	 (count-when
	  (fn [c] (some #(> % 0) c))
	  data)))))

(deftest counts-when-test
  (let [data {[0 6 0 2 4] 4
	      [0 6 0 2 3] 3
	      [0 6 0 2 2] 2 
	      [0 6 0 2 1] 1}]
    
  (is (= 7
	 (counts-when
	  (partial nth-is? 4 #(> % 2))
	  data)))
  (is (= 3
	 (counts-when
	  (partial nth-is? 4 #(< % 3))
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

(deftest make-maps-from-feature-vectors
  (is (=
       {0 {1 {1 1 2 3}
	    2 {1 1}}}
       (map-from-vectors
       [[0 1 1 1]
	[0 1 2 1]
	[0 1 2 1]
	[0 1 2 1]
	[0 2 1 1]])))
  (is (= {0 {1 {2 3}}}
	 (map-from-vectors [[0 1 2 3]]))))

(deftest feature-into-map
  (is (= {1 {2 {3 4}}}
	 (into-nested-map [1 2 3 4]))))

(deftest marginalize-m-test
  (is (= [[0 1 2 2]]
	 (marginalize [2] [[0 1 2 2 2]]))))

(deftest marginalize-m-test
  (let [example {0 {1 {2 {2 2}}}}]
    (is (= [[0 1 2 2 2]]
	 (feature-vectors2 example)))
  (is (= {0.0 {1.0 {2.0 2.0}}}
	 (marginalize-map [2] {0 {1 {2 {2 2}}}})))))

(deftest feature-target-mutual-information
  (is (= [-0.07290559532005605 -0.07290559532005605
	  -0.07290559532005605 -0.07290559532005605]
	 (feature-target-mi (I 5 5)))))

(defn f-prime [x]
  (- (* 4 (pow x 3)) (* 9 (pow x 2))))

(defn f [x]
  (+ (- (pow x 4) (* 3 (pow x 3))) 2))

(defn within [x delta]
  #(not (or (< x (- % delta))
	    (> x (+ % delta)))))

(deftest gradient-decent-test
  (let [x 6
	step 0.01
	precision 0.00001]
    (is ((within 2.24996 0.00001)
	   (gradient-descent f step precision x)))))