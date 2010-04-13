(ns infer.cross-validation
  (:use infer.features)
  (:use infer.smoothing)
  (:use infer.linear-models)
  (:use [clojure.contrib.seq-utils :only [flatten]])
  (:use [clojure.contrib.map-utils :only [deep-merge-with]])
  (:use [infer.core :only [safe threshold-to map-map levels-deep all-keys]])
  (:use [infer.probability :only [bucket +cond-prob-tuples]]))

(defn probs-only
  "Compute probability from computed counts.
   This is division, you have to count up the proper numerator and denominator
   in your counting step."
  ([k a b] [k (probs-only a b)])
  ([a b] (safe / a b)))

(defn process-prob-map
  "Process probability maps using a provided report function. Note that you
   can't pass keys to reporter our you get double nested final level in map."
 [[a-and-b b] report]
 (into {}
  (for [[bkey bval] a-and-b]
   [bkey
    (if (map? bval)
      (process-prob-map [bval (b bkey)] report)
      (report bval b))])))

(defn model-from-maps
  "Creates a model from probability report maps."
  [prob-map]
  (process-prob-map prob-map probs-only))

(defn invert-map [m]
  (into {}
	(map (comp vec reverse) m)))
	
(defn most-likely
  "Computes the most likely class from a map of classes to class probability.

   => (most-likely {:a 0.6 :b 0.4})
   :a"
  [m]
  (let [imap (if (map? m)
	       (invert-map m)
	       (zipmap m (range 0 (count m))))
	likely-class (apply max (keys imap))]
    (imap likely-class)))

(defn confusion-matrix
  "Computes a confusion matrix from the counts on train and test data sets
   represented as maps. traverse map...as you get to the point of counts of
   actuals, replace the series of nested keys that lead to that point with the
   single key of the predicted"
  [trd tst]
  (apply deep-merge-with +
	 (flatten
	  ((fn each-level [tr ts]
	     (for [[k v] ts
		   :let [it (tr k)
			 can-predict (not (nil? it))]]
	       (if (= 1 (levels-deep v))
		 (if can-predict
		   {(most-likely it) v}
		   {:no-prediction v})
		 (if can-predict
		   (each-level it v)
		   (each-level {} v)))))
	   trd tst))))

(defn linear-model-confusion-matrix
  [trd tst]
  (let [score (fn [ts]
		{(trd (vec-but-last ts))
		 {(vec-last ts) 1}})]
  (apply deep-merge-with +
	 (map score tst))))

(defn nn-confusion-matrix
  [trd tst]
  (let [score (fn [ts]
		{(trd ts)
		 {(vec-last ts) 1}})]
  (apply deep-merge-with +
	 (map score tst))))

;;http://en.wikipedia.org/wiki/Cross-validation_(statistics)
;;TODO: n*k fold
;;leave one out cross validation
;;extra hold out testing?
;;easily M/R-able if needed for some models.
(defn cross-validate
  "takes a model, a training/optimization algorithm, a fitness/loss function.

Lastly, takes n seqs of input-output vectors (or feature-target if that's how you roll) to be used as training and test examples.

holds each seq of vectors out in turn as the test set, merges the rest as training, and performs n-way cross-validation.

TODO: for now you are left on your own to aggregate the losses after the fn returns, should we parameterize teh aggregator as well?
"
  [model train fitness examples]
    (pmap
     (fn [test-set]
      (let [training-set (remove #{test-set} examples)
	    trained (train model training-set)]
	(fitness trained test-set)))
	 examples))

(defn to-pmf [model training-set]
   (model
    (reduce +cond-prob-tuples training-set)))

(defn to-linear-model [model training-set]
   (apply
    model
    (extract-ys
     (apply concat training-set))))

(defn to-nn-model [model training-set]
   (model
     (apply concat training-set)))

(defn cross-validation-confusion-matrix
  "Takes a set of n joint PMFs, and holds each joint PMF out in turn as the test
   set. Merges the resulting n cross-validation matrices into a single matrix."
  [xs]
  (apply deep-merge-with +
    (cross-validate
     model-from-maps
     to-pmf
     #(confusion-matrix %1 (first %2))
     xs)))

(defn cross-validation-linear-model
  [xs]
  (let [feature-vecs (map (comp
			   #(feature-vectors % missing-smoother)
			   first)
			  xs)]
    (apply deep-merge-with +
	   (cross-validate
	    (fn [x y]
	      (bucket #(predict
			(ols-linear-model x y)
			%)
		      [0 1 2 3]))
	      to-linear-model
	      linear-model-confusion-matrix
	      feature-vecs))))

(defn cross-validation-kernel-smoother
  [xs]
  (let [feature-vecs (map (comp
			   #(feature-vectors % missing-smoother)
			   first)
			  xs)]
    (apply deep-merge-with +
	   (cross-validate
	    (fn [vecs]
	      (bucket (knn-smoother 10 vecs)
		      [0 1 2 3]))
	      to-nn-model
	      nn-confusion-matrix
	      feature-vecs))))