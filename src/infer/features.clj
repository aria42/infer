(ns infer.features
  (:import java.util.Random)
  (:use clojure.contrib.combinatorics)
  (:use clojure.contrib.seq-utils)
  (:use clojure.contrib.math)
  (:use clojure.set)
  (:use infer.measures)
  (:use infer.information-theory)
  (:use infer.probability)
  (:use infer.matrix)
  (:use infer.core)
  (:use [clojure.contrib.map-utils :only [deep-merge-with]])
  (:use clojure.set))

(defn nth-is? [i pred coll]
  (pred (nth coll i)))

;;confusing names - this one takes a vec of feature-vectors and sums the # of examples passing the pred
(defn count-when [pred coll]
  (count (filter pred coll)))

;;confusing names - this one takes a map of feature-vectors -> counts, and sums all the counts whose feature-vector-key passes the pred
(defn counts-when [pred coll]
  (sum (vals (filter (comp pred first) coll))))

;;TODO: check on all these vec operations.
;;what about pop on ecs and butlast?
(defn vec-but-last [s]
  (subvec s 0
	  (max 0 (- (count s) 1))))

(defn vec-last [s]
  (nth s (- (count s) 1)))

(defn rest-vec [v i]
 (if (> i (- (count v) 1))
   nil
  (subvec v i)))

(defn remove-at [i v]
  (vec (concat (subvec v 0 i)
	  (subvec v (+ i 1) (count v)))))

(defn heterogenious-group-by
  "Returns a sorted map of the elements of coll keyed by the result of
   f on each element. The value at each key will be a vector of the
   corresponding elements, in the order they appeared in coll."
  [f coll]
  (reduce
   (fn [ret x]
     (let [k (f x)]
       (assoc ret k (conj (get ret k []) x))))
   {} coll))

(defn equivalence-classes
  "Takes a map where key is class and value is a set of equivalence classes to
   the key class.  it then inverts the mapping so that you can look up classes
   that are equivalence classes of a new larger class.
  
   => (equivalence-classes {0 #{0 1}, 1 #{2, 3, 4}, 2 #{5 6}})
   {0 0, 1 0, 2 1, 3 1, 4 1, 5 2, 6 2}"
  [class-mappings]
  (into {} (for [[k v] class-mappings]
    (into {} (for [equiv v] [equiv k])))))

(defn merge-levels
  [class-mappings coll]
  (map-map
    #(apply deep-merge-with + (map second %))
    (heterogenious-group-by
      (fn [[k v]]
        (if-let [new-key (class-mappings k)]
          new-key
          k))
      coll)))

(defn merge-equivalence-classes
  "example
 (let [model-merger {2 bucket-eq-classes
            3 bucket-eq-classes
            6 bucket-eq-classes}
    count-merger {2 bucket-eq-classes
            3 bucket-eq-classes}]
    (map (fn [[modelcnts totalcnts]] [(merge-equivalence-classes model-merger modelcnts)
              (merge-equivalence-classes count-merger totalcnts)]) model)))"
  [class-mappings x]
  (letfn [(merger [coll levels]
        (let [merged (merge-levels (if-let [mapping (class-mappings levels)]
             mapping
             identity) coll)]
          (into {} (for [[k v] merged]
         (if (not (map? v))
           [k v]
           [k (merger v (+ 1 levels))])))))]
    (merger x 1)))

(defn flatten-seqs
  "Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns the lowest level sequential items as a sequence of sequences."
  [x]
  (let [contains-seq? (partial some sequential?)]
    (filter (complement contains-seq?)
	    (rest (tree-seq contains-seq? seq x)))))

(defn missing-smoother [k]
  (if (= k :missing) 0 k))

(defn feature-vectors
  ([vect m smoother]
     (pmap (fn [[k v]]
	    (let [next-vect (conj vect (smoother k))]
	      (cond (number? v)
		    (repeat v next-vect)
		    :otherwise
		    (feature-vectors next-vect v smoother))))
	  m))
  ([m smoother]
     (flatten-seqs (feature-vectors [] m smoother)))
  ([m]
     (feature-vectors m identity)))

(defn feature-vectors2
  ([vect m smoother]
     (pmap (fn [[k v]]
	    (let [next-vect (conj vect (smoother k))]
	      (cond (number? v)
		    (conj next-vect v)
		    :otherwise
		    (feature-vectors2 next-vect v smoother))))
	  m))
  ([m smoother]
     (flatten-seqs (feature-vectors2 [] m smoother)))
  ([m]
     (feature-vectors2 m identity)))

(defn downsample [sample-percent coll]
     (let [ra (Random.)]
       (filter (fn [x] (< (.nextDouble ra)
			  sample-percent)) coll)))

(defn into-nested-map [v]
  (let [rv (vec (reverse v))
	mapize (fn [m restks]
		 (if (not restks) m
		     (recur {(first restks) m}
			    (rest-vec restks 1))))]
    (mapize {(second rv) (first rv)} (rest-vec rv 2))))

(defn map-from-vectors
"turns the feature vectors back into a nested map representation."
[vecs]
(apply deep-merge-with +
       (map into-nested-map vecs)))

(defn joint-and-marginals-from-vectors
[vecs]
(loop [jm-vec [{}{}{}]
       splatted (map
	       #(apply vector
		 (into-nested-map
		  (conj % 1))
		   (map (fn [x] {x 1}) %))
	       vecs)]
  (if (empty? splatted) jm-vec
      (recur
       (doall (map
	(fn [agg next]
	  (deep-merge-with + agg next))
	jm-vec (first splatted)))
       (rest splatted)))))
       
(defn vectors-as-keys
  "transforms the nested map representation into a vector-as-key with count-as-value representation."
  ([vect m]
     (for [[k v] m
	   :let [next-vect (conj vect k)]]
       (cond (number? v)
	     (conj next-vect v)
	     :otherwise
	     (vectors-as-keys next-vect v))))
  ([m] (into {}
	     (map #(vector
		    (vec-but-last %)
		    (vec-last %))
		  (flatten-seqs
		   (vectors-as-keys [] m))))))

(defn extract-ys [vs]
  [(pmap vec-last vs)
   (pmap vec-but-last vs)])

(defn seq-trans [seqs]
"transpose a seq of seqs.  useful to transform multi-class output vectors into vectors of outputs for each class."
  (apply map list seqs))

(defn marginalize [indices vecs]
  (let [A (matrix vecs)]
	(from-matrix (delete-columns A indices))))

(defn marginalize-vecs [index vecs]
 (pmap #(remove-at index %) vecs))

(defn marginalize-map [n m]
 (map-from-vectors
	(marginalize n (feature-vectors2 m missing-smoother))))