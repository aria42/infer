(ns infer.linear-models
  (:import [org.apache.commons.math.stat.regression
	    OLSMultipleLinearRegression
	    GLSMultipleLinearRegression])
  (:use infer.matrix))

(defn vecize-1d
"if this is the 1d case, put each calue in a vec."
[xs]
  (if (coll? (first xs)) xs
      (map vector xs)))

(defn commons-ols-linear-model [ys xs*]
  (let [xs (vecize-1d xs*)]
    (doto
	(OLSMultipleLinearRegression.)
      (.newSampleData (double-array ys)
		      (doubles-2d xs)))))

(defn commons-gls-linear-model [ys xs* sigma]
  (let [xs (vecize-1d xs*)]
    (doto
	(GLSMultipleLinearRegression.)
      (.newSampleData (double-array ys)
		      (doubles-2d xs)
		      (doubles-2d sigma)))))

(defn commons-betas [m]
"get the betas from an OLSMultipleLinearRegression model."
  (into [] (.estimateRegressionParameters m)))

(defn ols-linear-model [ys xs]
 (let [X (matrix xs)
       Y (matrix ys)
       Xt (trans X)
       XtX (times Xt X)
       XtY (times Xt Y)]
       (times (inv XtX) XtY)))

;;TODO: should we pull this out of matrix form with from-matrix and flatten?
;;>[[60.34103261134334] [60.34103261134334]] (from-matrix (predict r1 [1.6]))))
;;>[[60.34103261134334]] (from-matrix (predict r1 [1.6]))))
(defn predict [B xs]
  (let [X (matrix xs)]
    (times (trans B) X)))

(defn gls-linear-model [ys xs sigma]
 (let [X (matrix xs)
       Y (matrix ys)
       S (matrix sigma)
       Si (inv S)
       Xt (trans X)
       XtSi (times Xt Si)
       XtSiX (times XtSi X)
       XtSiY (times XtSi Y)]
       (times (inv XtSiX) XtSiY)))

(defn weighted-linear-model [ys xs weights]
  (gls-linear-model ys xs (to-diag weights)))