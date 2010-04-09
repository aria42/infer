(ns infer.classification
  "Fundamentals of classification, cross validation, and performance measures
   such as precision and recall.

   Classifiers are maps of classifier-name -> functions, data are maps of
   feature-name features."
  (:use infer.features)
  (:use infer.weka-spike)
  (:use infer.smoothing)
  (:use infer.linear-models)
  (:use [clojure.contrib.seq-utils :only [flatten]])
  (:use [clojure.contrib.map-utils :only [deep-merge-with]])
  (:use [infer.core :only [safe threshold-to map-map levels-deep all-keys]])
  (:use [infer.probability :only [bucket +cond-prob-tuples]]))

(defn discretizing-classifiers
  "Makes a discretizing classifier out of each key-range pair."
  [ranges]
  (for [[k range] ranges]
    (bucket k range)))

(defn classify-one-to-one
  "Takes a map of fns and a map of features, where there is one classifier fn per feature and they share the same key names in the classifier and feature maps.  apply each classifer fn to the corresponding feature.

   => (classify-one-to-one
        {:a (present-when (gt 5)) :b (present-when (lt 5))}
        {:a 10 :b 5})
   {:a 1 :b 0}"
  [fns data]
  (merge-with #(%1 %2) fns data))

(defn classify-one-to-each
  "Takes a map of fns and a map of features, apply each classifer fn to each feature in the data map individually.
 
   => (classify-one-to-each
        {:a (present-when (gt 5)) :b (present-when (lt 5))}
        {:a 10 :b 5})
   {:a {:a 1 :b 0} :b {:a 0 :b 0}}"
  [fns data]
  (into {} (for [[f-k f] fns] [f-k (map-map f data)])))

(defn classify-one-to-all
  "Takes a map of fns and a map of features, apply each classifer fn to the entire feature map.

  => (classify-one-to-each
       {:a (present-when #(and (> (:a %) 9) (< (:b %) 6)))}
       {:a 10 :b 5})
  {:a 1}"
  [fns data]
  (into {} (for [[f-k f] fns] [f-k (f data)])))

(defn classification-workflow
  "Composes a classification workflow from a classifier a counter and a
   transformer. Note that count-all has been abstracted due to the fact that you
   may count with reduce or merge-with depending on wheter you ahve vectors or
   maps."
  [transformer classifier count-all]
  (fn [obs] (count-all (map classifier (transformer obs)))))

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

(defn precision
  "Computes precision by class label from confusion matrix."
  [m]
  (into {}
  (for [[k v] m
        :let [predicted (v k)]
        :when (not (= k :no-prediction))]
    [k (float (/ (if predicted predicted 0)
    (apply + (vals v))))])))

(defn recall
  "Computes recall by class label from confusion matrix."
  [m]
  (into {}
  (for [k (all-keys m)
        :let [v-predicted (m k)]
        :when (not (or (= k :missing)
           (= k :no-prediction)))]
    [k
     (if (not v-predicted) 0
         (let [the-prediction (v-predicted k)]
     (float (/ (if the-prediction the-prediction 0)
         (apply + (for [[k-actual v-actual] m]
              (threshold-to 0
                (v-actual k))))))))])))

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

(defn cross-validation-logistic-regression
  [xs]
  (let [feature-vecs (map (comp
			   #(feature-vectors % missing-smoother)
			   first)
			  xs)]
    (apply deep-merge-with +
	   (cross-validate
	    (fn [x y]
	      (bucket #(classify
			(logistic-regression x y)
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

(defn n-times-k-fold-cross-validation-confusion-matrix
  [list-of-lists]
  (apply deep-merge-with +
  (map (partial apply cross-validation-confusion-matrix)
       list-of-lists)))