;; (ns infer.weka-spike
;;   (:use infer.matrix)
;;   (:import [com.aliasi.stats AnnealingSchedule LogisticRegression RegressionPrior])
;;   (:import [com.aliasi.matrix Vector DenseVector]))
;; ;;  (:import [org.jdmp.weka.classifier WekaClassifier WekaClassifier$WekaClassifierType]))

;; ;; (defn weka-logistic [Y X W]
;; ;;  (let [c (WekaClassifier.
;; ;; 	  (WekaClassifier$WekaClassifierType/Logistic) true (into-array String []))]
;; ;;    (.train c X W Y)))

;; ;; (defn logistic-spike []
;; ;;   (let [X (fill-rand 20 2)
;; ;; 	Y (matrix (range 1 11)) 
;; ;; 	W (matrix (repeat 10 1))]
;; ;;     (weka-logistic Y X W)))


;; (defn dense-vector [v]
;;   (DenseVector. (double-array v)))

;; (defn vectors [inputs]
;;   (into-array Vector (map dense-vector inputs)))

;; (def test-outputs (int-array [
;;         1,
;;         1,
;;         2,
;;         2,
;;         0]))

;; (def test-inputs (vectors [[1, 0, 0, 2, 0]
;; 			  [1, 0, 0, 2, 1]
;; 			  [1, 0, 0, 1, 1]
;; 			  [1, 0, 0, 2, 0]
;; 			  [1, 1, 0, 1, 1]]))

;; (defn logistic-regression [outputs inputs]
;;   (LogisticRegression/estimate (vectors inputs)
;; 			       (int-array outputs)
;;                                       (RegressionPrior/noninformative)
;;                                       (AnnealingSchedule/inverse 0.05 100)
;;                                       nil ;;null reporter        
;;                                       0.000000001 ;; min improve
;;                                       1  ;;min epochs
;; 	                              10000));; max epochs

;; (defn lingpipe-classify [model xs]
;; (into [] (.classify model (dense-vector xs))))