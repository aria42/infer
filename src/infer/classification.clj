(ns infer.classification
  "Fundamentals of classification, cross validation, and performance measures
   such as precision and recall.

   Classifiers are maps of classifier-name -> functions, data are maps of
   feature-name features."
  (:use infer.features)
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