(ns infer.trees)

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
