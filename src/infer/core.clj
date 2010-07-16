(ns infer.core
  (:import org.apache.commons.math.util.MathUtils)
  (:use clojure.contrib.monads)
  (:use [clojure.set :only [intersection]]))

;;TODO: find tests for this stuff.

(defn pow [x y] (Math/pow x y))

(def pi Math/PI)

(def e Math/E)

(defn exp [x] (Math/pow e x))

(defn log2 [x]
  (MathUtils/log 2 x))

(defn ln [x]
  (MathUtils/log e x))

(defn sign [x] (if (>= x 0) 1 -1))

(defn cos [x] (Math/cos x)) 
(defn acos [x] (Math/acos x)) 
(defn sin [x] (Math/sin x)) 
(defn asin [x] (Math/asin x)) 
(defn tan [x] (Math/tan x)) 
(defn atan [x] (Math/atan x)) 

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

; TODO: prune this old infer.core code

(defn same-length? [a b]
  (= (count a) (count b)))

(defn table-to-vectors
  "Takes a big vector that is composed of two vectors of alternating membership
   in the super vector and splits out the individual vectors.

   [106 	7
   86 	0
   100 	27
   101 	50
   99 	28
   103 	29
   97 	20
   113 	12
   112 	6
   110 	17]

   [[106 86 100 101 99 103 97 113 112 110]
    [7 0 27 50 28 29 20 12 6 17]]"
  [z]
  (reduce
    (fn [[x y][x1 y1]]
      [(conj x x1) (conj y y1)])
    [[][]]
    (partition 2 2 z)))

(defn map-map [f x]
  (into {} (for [[k v] x] [k (f v)])))

(defn map-from-keys [a f]
  (into {} (for [k a] [k (f k)])))

(defn map-from-pairs [a f]
  (into {} (for [[k v] a] [k (f k v)])))

(defn map-from-nested-map [a f]
  (into {} (for [[k v] a] [k (map-map f v)])))

(defn set-to-unit-map [s]
  (apply hash-map (interleave s (repeat (count s) 1))))

(defn key-compare
 [x y]
 (cond
   (and (keyword? x) (not (keyword? y)))
     1
   (and (keyword? y) (not (keyword? x)))
     -1
   :else
     (compare x y)))

;;weird inversion makes us revers k1 and k2
(defn kv-compare [[k1 v1] [k2 v2]]
  (key-compare k2 k1))

;;TDOO: doesn't seem to work? test and beat on it.
;;use clojrue sorting: sort-by, sorted-map-by, etc.
(defn sort-map [m]
  (into {} (sort kv-compare m)))

(defn map-compare [k]
  #(compare (k %1) (k %2)))

(defn sort-maps-by [k maps]
  (sort (map-compare k) maps))

(defn sort-map-of-maps [m]
  (sort-map
  (for [[k v] m]
     [k (if (map? v)
	  (sort-map-of-maps v)
	  v)])))

(defn all-keys
  "Teturns a set of all the keys from an arbitarily deeply nested map or seq of
   maps."
  [m]
  (cond (not (map? m))
	  (apply intersection (map all-keys m))
	:else
	  (into #{}
	      (flatten
	       ((fn get-keys [x]
		  (cons (keys x)
			(map get-keys
			     (filter map? (vals x)))))
		m)))))

(defn bottom-level?
  "Given a map; is this the bottom level in the map?

   (bottom-level? {:a 1}) -> true
   (bottom-level? {:a {:b 1}}) -> false"
  [m]
  (not (map? (second (first m)))))

(defn levels-deep
  "Returns the number of levels of depth of nesting for a nested map.

   1  -> 0
   {} -> 0
   {0 1} -> 1
   {1 {0 1}} -> 2"
  [m]
  (apply max 0
	 (flatten
	  ((fn count-level [m n]
	     (if (not (coll? m)) 0
	     (for [[k v] m]
	       (if (map? v)
		 (count-level v (+ n 1))
		 n))))
	   m 1))))

(defn flatten-with
  "Takes an arbitrarily deeply nested map, and flattens it to one level by
   merging keys.

   (flatten-with str {:a {:b {:c 1}}}) -> {\":a:b:c\" 1}"
  [f nested-map]
  (apply hash-map
  (flatten
  ((fn flatten-level [key-acc m]
  (for [[k v] m
        ;;TODO: really shady ass use of nil for conditional, better impl?
        :let [new-key (if key-acc (f key-acc k) k)]]
        (if (not (map? v))
  	[new-key v]
        (flatten-level new-key v)))) nil nested-map))))
