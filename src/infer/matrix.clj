(ns infer.matrix
  (:use infer.features)
  (:import java.util.Random)
  (:import [org.ujmp.core Matrix
	    MatrixFactory
	    Ops])
  (:import [org.ujmp.core.matrix Matrix2D])
  (:import [org.ujmp.core.doublematrix DenseDoubleMatrix2D DoubleMatrix2D])
  (:import org.ujmp.core.doublematrix.calculation.general.decomposition.Chol))

(defn ensure-vecs [xs]
  (if (and (vector? xs) (vector? (first xs)))
    xs
    (vec (map vec xs))))

(defn doubles-2d [xs]
  (let [vecs (ensure-vecs xs)
	#^"[[D" arr
	(make-array Double/TYPE (count vecs) (count (first vecs)))]
    (dotimes [idx (count vecs)]
	   (aset arr (int idx)
		 #^doubles (double-array (nth vecs idx))))
    arr))

(defn matrix [xs]
   (MatrixFactory/importFromArray #^"[[D" (doubles-2d xs)))

(defn fill [v r c]
  (MatrixFactory/fill v (long-array [r c])))

(defn rand-elems [r c] 
  (let [ra (Random.)]
    (partition c (repeatedly (* c r) #(.nextDouble ra)))))

(defn fill-rand [r c]
  (matrix (rand-elems r c)))

(defn get-at [#^DoubleMatrix2D m r c]
  (.getDouble m (int r) (int c)))

(defn set-at [#^DoubleMatrix2D m v r c]
 (.setDouble m (double v) (int r) (int c)))

(defn inc-at
  ([m r c] (inc-at m 1 r c))
  ([m by r c]
     (set-at m
	     (+ (get-at m r c) by)
	     r c)
     m))

(defn times [A B]
  (.mtimes #^DenseDoubleMatrix2D A #^DenseDoubleMatrix2D B))

(defn trans [A]
  (.transpose #^DenseDoubleMatrix2D A))

(defn svd [A]
  (.svd #^DenseDoubleMatrix2D A))