(ns infer.features
  (:use clojure.contrib.combinatorics)
  (:use clojure.contrib.seq-utils)
  (:use clojure.set))

(defn flatten-seqs
  "Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns the lowest level sequential items as a sequence of sequences."
  [x]
  (let [contains-seq? (partial some sequential?)]
    (filter (complement contains-seq?)
	    (rest (tree-seq contains-seq? seq x)))))

(defn feature-vectors
  ([vect m]
      (for [[k v] m
	    :let [next-vect (conj vect k)]]
        (cond (number? v)
	  (repeat v next-vect)
	  :otherwise
	  (feature-vectors next-vect v))))
  ([m] (flatten-seqs (feature-vectors [] m))))

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

(defn extract-ys [vs]
  [(map last vs)
   (map vec-but-last vs)])

(defn seq-trans [seqs]
"transpose a seq of seqs.  useful to transform multi-class output vectors into vectors of outputs for each class."
  (apply map list seqs))
