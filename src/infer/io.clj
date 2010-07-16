(ns infer.io
  (:use clojure.contrib.duck-streams)
  (:use clojure-csv.core)
  (:use infer.matrix))

(defn csv->matrix [path]
  (let [strings (parse-csv (slurp path))]
    (matrix (for [row strings
		  :when (not (some #(= "" %) row))]
		   (map #(Float/parseFloat  %) row)))))