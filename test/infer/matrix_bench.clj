(ns infer.matrix-bench
  (:import java.util.Random)
  (:use infer.matrix))

;;http://measuringmeasures.com/blog/2010/3/28/matrix-benchmarks-fast-linear-algebra-on-the-jvm.html
 
(defn rand-sparse-rows
"note that when we create random column indices, we do not ensure no collisions.
that can happen and just results in slighly higher sparsity % than s for that row."
[r c s]
  (let [sparse-elems   (rand-elems r (* s c))
	rand-c #(rand-int (+ c 1))]
    (map
     (fn [row]
       (into {} (map (fn [x] [(rand-c) x]) row)))
     sparse-elems)))

(defn bench-matrix [n op A B]
  (time (dotimes [i n] (op A B))))

(defn bench-dense [n op rA cA rB cB]
  (let [a (matrix (rand-elems rA cA))
	b (matrix (rand-elems rB cB))]
    (bench-matrix n op a b)))

(defn bench-sparse [n op rA cA rB cB s] 
  (let [a (sparse-matrix (rand-sparse-rows rA cA s))
	b (sparse-matrix (rand-sparse-rows rB cB s))]
    (bench-matrix n op a b)))

(defn bench-sparse-colt [n op rA cA rB cB s] 
  (let [a (sparse-colt-matrix (rand-sparse-rows rA cA s))
	b (sparse-colt-matrix (rand-sparse-rows rB cB s))]
    (bench-matrix n op a b)))

(defn bench-sparse-pcolt [n op rA cA rB cB s] 
  (let [a (sparse-pcolt-matrix (rand-sparse-rows rA cA s))
	b (sparse-pcolt-matrix (rand-sparse-rows rB cB s))]
    (bench-matrix n op a b)))

(defn bench-all [[n op rA cA rB cB s :as args]]
  (cons (bench-dense n op rA cA rB cB)
	(doall (map #(apply % args)
		    [bench-sparse
		     bench-sparse-colt
		     bench-sparse-pcolt]))))