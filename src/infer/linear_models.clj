(ns infer.linear-models
  (:use infer.core)
  (:use infer.matrix)
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

(defn irls [Y X Bold precision]
  (let [
	yhat (times X Bold)
 	P (map #(/ 1 (+ 1 (exp (- %))))
	       (from-column-matrix yhat))
	weights (map #(* % (- 1 %)) P)
	W (matrix (to-diag weights))
	z (plus yhat
		(times (inv W)
		       (minus Y (column-matrix P))))
	;Bnew (gls-linear-model z X W)]
	; Temporary fix for irls ... need to rework GLS and use those results.
	Bnew (times (inv (times (trans X) W X )) (times (trans X) W z)) ]
    (if (<= (euclidean-distance (from-column-matrix Bnew)
				(from-column-matrix Bold)) precision) Bnew
	(recur Y X Bnew precision))))