(ns infer.trees
  (:use infer.core)
  (:use infer.measures))

(defn leaf
"takes an index i into a vector v.
 gets the value of the ith element of v.
 tests the value with predicate p.
 returns the value l for true and r for false."
 [i p l r]
  (fn [v]
    (if (p (v i)) l r)))

(defn node
"takes an index i into a vector v.
 gets the value of the ith element of v.
 tests the value with predicate p.
 returns the application of l to value for true and r to value for false."
  [i p l r]
  (fn [v]
    (if (p (v i)) (l v) (r v))))

(defn midpoints [points]
  (map (comp mean vector)
       (rest points) points))

(defn count-split
[region i s]
  (let [l (count (filter #(<= (nth % i) s) region))
	r (- (count region) l)]
    [l r]))

(defn count-within
  ([region i l r] 
     (count (filter #(and (>  (nth % i) l)
			  (<= (nth % i) r)) region))))

;;TODO: this only does the counts term of the computiton, we still need to add mean y vals term.
(defn best-split
([r]
   (map #(best-split r %)
	(range 0 (count (first r)))))
([r i]
   (let [values  (into #{} (map #(nth % i) r))
	 counts (map #(count-split r i %) values)
	 tot (count r)
	 gain (map
	       (fn [[l r] v]
		 [v (/ (* l r) tot)])
	       counts values)]
     (apply vector i (max-by second gain)))))
  