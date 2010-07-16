(ns infer.linear-models
  (:use clojure.contrib.math)
  (:use infer.core)
  (:use infer.matrix)
  (:use infer.learning)
  (:use infer.measures)
  (:use infer.probability))

(defn vecize-1d
"if this is the 1d case, put each calue in a vec."
[xs]
  (if (coll? (first xs)) xs
      (map vector xs)))

;;TODO: should we pull this out of matrix form with from-matrix and flatten?
;;>[[60.34103261134334] [60.34103261134334]] (from-matrix (predict r1 [1.6]))))
;;>[[60.34103261134334]] (from-matrix (predict r1 [1.6]))))
(defn predict [B X]
    (times (trans B) X))

(defn classify [B xs classes]
  (range-classifier classes
;;TODO: hardcoded for single label, single prediction
		    (first (first (from-matrix (predict B xs))))))

;;TODO: numericaly better here to use cholesky, svd, or qr?
(defn ols-linear-model [Y X]
 (let [[Q R] (qr X)
       Qt (trans Q)
       QtY (times Qt Y)]
       (times (inv R) QtY)))

(defn gls-linear-model [Y X S]
  (let [Li (inv (chol S))
       XLi (times Li X)
	YLi (times Li Y)]
       (ols-linear-model YLi XLi)))

(defn weighted-linear-model [Y X weights]
  (gls-linear-model Y X (matrix (to-diag weights))))

;;TODO: use approaches in article above or cross validation for selecting lambda.
;;TODO: maybe rename all "linear-model"s above to be "regression" for api consistency? 
;;TODO: numericaly better to use svd here or cholesky? 
(defn ridge-regression
"
http://en.wikipedia.org/wiki/Tikhonov_regularization
"
[Y X lambda]
 (let [p (column-count X)
       [U D Vt] (svd X)
       penalty (times (I p p) lambda)
       D* (times (inv (plus (times D D) penalty)) D)
       VtD* (times (trans Vt) D*)
       UtY (times (trans U) Y)]
       (times VtD* UtY)))

(defn irls [Y X linkfn varfn Bold precision]
  (let [yhat (times X Bold)
 	P (map linkfn
	       (from-column-matrix yhat))
	weights (map varfn P)
	W (matrix (to-diag weights))
	z (plus yhat
		(times (inv W)
		       (minus Y (column-matrix P))))
	;Bnew (gls-linear-model z X W)]
	; Temporary fix for irls ... need to rework GLS and use those results.
	Bnew (times (inv (times (trans X) W X )) (times (trans X) W z)) ]
    (if (<= (euclidean-distance (from-column-matrix Bnew)
				(from-column-matrix Bold)) precision) Bnew
	(recur Y X linkfn varfn Bnew precision))))

(defn lasso [Y X Bold lambda precision]
  (let [inner (fn [Blast j]
		 (let [Xj (select-columns X [j])
		       Rj (plus
			   (minus Y
				  (times X Blast))
			   (times Xj (get-at Blast j 0)))
		       Bj* (/ (times (trans Xj) Rj)
			      (row-count X))
		       Bjnew (* (sign Bj*)
				    (max 0 (- (abs Bj*) lambda)))
		       _ (set-at Blast Bjnew j 0)]
		   (if (= 0 j) Blast
		       (recur Blast (- j 1)))))]
  (coordinate-descent inner Bold active-set-convergence? (- (column-count X) 1))))