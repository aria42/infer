(ns infer.features
  (:use clojure.contrib.combinatorics)
  (:use clojure.contrib.seq-utils)
  (:use [infer.core :only [map-map]])
  (:use [clojure.contrib.map-utils :only [deep-merge-with]])
  (:use clojure.set))

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

(defn vectors-as-keys
"transforms the nested map representation into a vector-as-key with count-as-value representation."
  ([vect m]
      (for [[k v] m
	    :let [next-vect (conj vect k)]]
        (cond (number? v)
	  (conj next-vect v)
	  :otherwise
	  (vectors-as-keys next-vect v))))
  ([m] (flatten-seqs (vectors-as-keys [] m))))

(defn remove-at [i v]
  (concat (subvec v 0 i)
	  (subvec v (+ i 1) (count v))))

(defn nth-is? [i pred coll]
  (pred (nth coll i)))

(defn count-when [pred coll]
  (count (filter pred coll)))
              
(defn vec-but-last [s]
  (subvec s 0 
	  (max 0 (- (count s) 1))))

(defn vec-last [s]
  (nth s (- (count s) 1)))

(defn extract-ys [vs]
  [(pmap vec-last vs)
   (pmap vec-but-last vs)])

(defn seq-trans [seqs]
"transpose a seq of seqs.  useful to transform multi-class output vectors into vectors of outputs for each class."
  (apply map list seqs))
