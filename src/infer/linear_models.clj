(ns infer.linear-models
  (:import org.apache.commons.math.stat.regression.OLSMultipleLinearRegression)
  (:use infer.matrix))

(defn ols-linear-model [ys xs*]
  (let [xs (if (coll? (first xs*)) xs*
	     (map vector xs*))]
    (doto
	(OLSMultipleLinearRegression.)
      (.newSampleData (double-array ys)
		      (doubles-2d xs)))))

(defn betas [m]
"get the betas from an OLSMultipleLinearRegression model."
  (into [] (.estimateRegressionParameters m)))

(defn my-ols-linear-model [ys xs]
 (let [X (matrix xs)
       Y (matrix ys)
       Xt (trans X)
       XtX (times Xt X)
       XtY (times Xt Y)]
       (times (inv XtX) XtY)))

(defn predict [B xs]
  (let [X (matrix xs)]
    (times (trans B) X)))  
	