(ns infer.features
  (:import org.apache.commons.math.stat.regression.OLSMultipleLinearRegression))

(defn ols-linear-model [ys xs]
  (doto
      (OLSMultipleLinearRegression.)
    (.newSampleData (double-array ys)
		    (double-matrix xs))))

(defn betas [m]
"get the betas from an OLSMultipleLinearRegression model."
  (into [] (.estimateRegressionParameters m)))
