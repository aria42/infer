(ns infer.learning
  (:use clojure.set)
  (:use clojure.contrib.math)
  (:use infer.matrix)
  (:use infer.measures)
  (:use infer.probability)
  (:use infer.information-theory)
  (:use infer.features))

;;optimization, regularization, and subset selection

(defn feature-target-pairs
([A] (feature-target-pairs A (- (column-count A) 1)))
([A target]
    (pmap (fn [feature]
	    (select-columns A [feature target]))
	  (remove (eq target)
			  (range 0 (column-count A))))))

(defn feature-target-mi [A]
  (pmap (comp
	#(mutual-information (first %) (rest %))
	joint-and-marginals-from-vectors
	from-matrix)
       (feature-target-pairs A)))

(defn matrix-map [f c]
  (apply map
	 (fn [& args]
	   (map f args)) c))

(defn feature-mi-matrix [A]
  (matrix-map
   (comp
	#(mutual-information (first %) (rest %))
	joint-and-marginals-from-vectors
	from-matrix)
       (map
	#(feature-target-pairs A %)
	(range 0 (column-count A)))))

(defn index-of-max [v]
  (loop [max-v (first v)
	 max-i 0
	 i 0
	 next v]
    (let [x (first next)
	  xs (rest next)
	  next-i (inc i)
	  new-max (max x max-v)
	  new-i (if (> new-max max-v) i max-i)]
    (if (empty? xs)
      new-i
      (recur new-max new-i next-i xs)))))

;;watch out for the ordering of columns that are selected from the matrix for the mi matrix.
(defn mrmr-feature-set
"k: # of features to select
t: index of target
vecs: feature-target vectors" 
[k t vecs]
(let [A (matrix vecs)
      AI (feature-mi-matrix A)
      Ixy (nth AI t)
      initial (index-of-max Ixy)
      S* [initial]
      s (count AI)
      fs (range 0 s)
      mrmr (fn [S]
;;repeat until s = k
	     (if (= k (count S)) S
;;find max: ( I(xi,y) - 1/S * sum of I(xi, xj))
	     (let [xis (difference S fs)
		   goal (map 
			 (fn [xi]
			   (- (nth Ixy xi)
			      (* (/ 1 s)
				 (sum (map (nth
					    (nth I xi)
					    xi)
					   S))))) 
			   xis)]
;;move best into set of selected
		   (recur (conj S (index-of-max goal))))))]
  (mrmr S*)))

(defn dydx [f x1 x0]
  (/ (- (f x1) (f x0))
     (- x1 x0)))

;; http://en.wikipedia.org/wiki/Gradient_descent
;; A more robust implementation of the algorithm would also check whether the function value indeed decreases at every iteration and would make the step size smaller otherwise. One can also use an adaptive step size which may make the algorithm converge faster.
(defn gradient-descent
([f step precision x]
   (gradient-descent f step precision x 0)) 
([f step precision x1 x0]
  (if (> (abs (- x1 x0)) precision)
    (recur f step precision (- x1 (* step (dydx f x1 x0))) x1)
	   x1)))

;; (defn newton-step [X Bnext weights]
;;   (let [mu (invlink eta)
;;         z (plus (mult X Bnext) (mult (minus y mu) (dlink mu)))
;;         W (diag weights)]
;;         (mult 
;;          (solve (mult (trans X) W X)) 
;;          (trans X) W z)))

;;http://en.wikipedia.org/wiki/Simulated_annealing
;;simulated annealing
;; s ← s0; e ← E(s)                                // Initial state, energy.
;; sbest ← s; ebest ← e                            // Initial "best" solution
;; k ← 0                                           // Energy evaluation count.
;; while k < kmax and e > emax                     // While time left & not good enough:
;;   snew ← neighbour(s)                           // Pick some neighbour.
;;   enew ← E(snew)                                // Compute its energy.
;;   if enew < ebest then                          // Is this a new best?
;;     sbest ← snew; ebest ← enew                  // Save 'new neighbour' to 'best found'.
;;   if P(e, enew, temp(k/kmax)) > random() then   // Should we move to it?
;;     s ← snew; e ← enew                          // Yes, change state.
;;   k ← k + 1                                     // One more evaluation done
;; return sbest                                    // Return the best solution found.

;;irls

;;lars





;;next up
;;http://en.wikipedia.org/wiki/Regularization_(mathematics)
