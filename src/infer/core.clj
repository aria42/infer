(ns infer.core
  (:use clojure.contrib.monads))

;;TODO: find tests for this stuff.


;;TODO: this doesn't have to be macros if the m-lift is not a macro
(defn lift-apply [f args]
  ((m-lift 1 (partial apply f)) args))

(defn any? [p c]
  (if (some p c)
    true
    false))

(defn nil-coll? [mv] 
  (or (nil? mv) 
      (and (coll? mv)
	   (any? nil? mv))))

;;TODO: refactor based on email exchange with Konrad.
(defmonad maybe-seq-m 
   [m-zero   nil
    m-result (fn m-result-maybe [v] v)
    m-bind   (fn m-bind-maybe [mv f]
               (if (nil-coll? mv)
		 nil 
		 (f mv)))
    m-plus   (fn m-plus-maybe [& mvs]
	       (first (drop-while nil-coll? mvs)))])

;;TODO: this doesn't have to be macros if the m-lift is not a macro might be able to do eval trickery
(defn maybe? [pred & args]
 (or 
   (with-monad maybe-seq-m
     ((m-lift 1 (partial apply pred)) args))
  false))
;;      (lift-apply pred args)))

;;TODO: combine into one tree comp that can figure out if it should call one branch function on each leave, or each branch function on all leaves.
(defn tree-comp-each [root branch & leaves]
 (apply 
  root (map branch leaves)))

(defn tree-comp  [root & branches] 
  (fn [& leaves] 
    (with-monad maybe-seq-m
    ((m-lift 1 (partial apply root))
      (map 
       (fn [branch] 
	 (if (ifn? branch)
	   ((m-lift 1 (partial apply branch)) leaves)
	   branch)) 
       branches))))) 

(defn all [& fs] 
  (apply tree-comp 
	 (fn [& xs] 
	   (eval (conj xs 'and))) 
	 fs))

(def both all)
(defn either [f g] (tree-comp (fn [a b] (or a b)) f g))  
(defn neither [f g] (tree-comp (fn [a b] (not (or a b))) f g))  

(defn cond-comp [p a b]
  (fn [x] 
    (if (p x) (a x) (b x))))

(defn makekey [ks obs] 
  (apply str (map #(% obs) ks)))

(defn vector-comp 
"compose a list of functions such that they are each applied to the arguments to which the composed function is applied and the results of each application are inserted as slots in a vector."
[& fns]
  (fn [& args] 
    (into [] 
	  (map #(apply % args) fns))))

(defn first-match [pred coll]
  (first (filter pred coll)))

(defn seqify [x]
  (let [colled (if (coll? x) x [x])]
     (seq colled)))

(defn seqable? [x] (or (seq? x) (string? x)))

(defn nil-or-empty [coll]
  (or (nil? coll) 
      (and (seqable? coll) (empty? coll))))

(defn all-present? [ks m] 
(not-any? nil-or-empty (map #(get m %) ks)))

;;TODO: all the safe stuff needs refacotring and generalization
;;should form a coherent system with above monadic compositions.
(defn safe
  "for safe division - returns zero for division by zero"
  [f n d] 
  (if (= d 0) 
    0.0 
    (float (f n d))))

(defn safe-max [x]
  (let [res (filter (complement nil?) (seqify x))]
    (apply max (if (empty? res) 0 res))))

(defn safe-max-date [x]
  (let [res (filter (complement nil?) (seqify x))]
    (last (sort res))))
     
(defn threshold-to [threshold x]
  (safe-max (conj (seqify x) threshold)))

(defn seqable? [x] 
  (or (coll? x) (string? x)))

(defn nil-or-empty? 
[coll] 
  (or (nil? coll) 
      (and (seqable? coll) 
	   (empty? coll))))

(defn all-present? [ks m] 
  (not-any? nil-or-empty? 
	    (map #(get m %) ks)))

(defn r-acc [look? extract x]
  "usage
     - (r-acc map? :a [:a 1 2 3 {:a 1 :b [:a 2 3] :c {:b 1 :a 2}} 3 #{:a :b 1} {:b {:a 3}}])
       (1 2 3)"
  (letfn [(children [coll] (if (map? coll) (vals coll) coll))]
    (filter (complement nil-or-empty?) 
	    (map extract 
		 (filter look? 
			 (tree-seq coll? children x))))))

(defn duplicates? [x]
  (not (= (count (distinct x))
	  (count x))))
