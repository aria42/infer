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

(def mi-from-matrix
     (comp
      #(mutual-information (first %) (rest %))
      joint-and-marginals-from-vectors
      from-matrix))

(defn feature-target-mi [A]
  (pmap mi-from-matrix 
       (feature-target-pairs A)))

(defn feature-mi-matrix [A]
  (pmap
   #(pmap mi-from-matrix (feature-target-pairs A %))
   (range 0 (column-count A))))

(defn- max-by [keyfn coll]
  (if (empty? coll) nil
      (let [maxer (fn [max-elem next-elem]
		    (if (> (keyfn max-elem) (keyfn next-elem))
		      max-elem
		      next-elem))]
	(reduce maxer coll))))

(defn index-of-max [v indices]
 (second (max-by first (map vector v indices))))

;;watch out for the ordering of columns that are selected from the matrix for the mi matrix.
(defn mrmr-feature-set
"k: # of features to select
t: index of target
vecs: feature-target vectors" 
[k t vecs]
(let [A (matrix vecs)
      AI (feature-mi-matrix A)
      Ixy (nth AI t)
      fs (remove (eq t) (range 0 (count AI)))
      initial (index-of-max Ixy fs)
      S* [initial]
      mrmr (fn [S]
;;repeat until s = k
	     (if (= k (count S)) S
;;find max: ( I(xi,y) - 1/S * sum of I(xi, xj))
	     (let [xis (difference (into #{} fs) (into #{} S))
		   goal (map 
			 (fn [xi]
			   (- (nth Ixy xi)
			      (* (/ 1 (count S))
				 (sum (map #(nth
					    (nth AI %)
					    xi)
					   S)))))
			   xis)]
;;move best into set of selected
		   (recur (conj S (index-of-max goal xis))))))]
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
