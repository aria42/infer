(ns infer.matrix
  (:use infer.features)
  (:import java.util.Random)
  (:import [org.ujmp.core Matrix
	    MatrixFactory
	    Ops])
  (:import [org.ujmp.core.matrix Matrix2D])
  (:import [org.ujmp.core.doublematrix DenseDoubleMatrix2D DoubleMatrix2D])
  (:import org.ujmp.core.calculation.Calculation$Ret)
  (:import org.ujmp.core.doublematrix.calculation.general.decomposition.Chol))

(defn ensure-vecs [xs]
  (let [to-vec #(if (vector? %) % (vec %))]
    (to-vec (map to-vec xs))))

(defn doubles-2d [xs]
  (let [vecs (ensure-vecs xs)
	#^"[[D" arr
	(make-array Double/TYPE (count vecs) (count (first vecs)))]
    (dotimes [idx (count vecs)]
	   (aset arr (int idx)
		 #^doubles (double-array (nth vecs idx))))
    arr))

(defn with-intercept [xs]
  (if (not (coll? (first xs))) ;;1 column
    (map #(vector 1 %) xs)
    (map #(vec (cons 1 %)) xs)))
  
(defn matrix [xs]
  (cond (not (coll? xs)) ;;already a matrix
	xs
	(not (coll? (first xs))) ;;1 column
	(matrix (map vector xs))
	:else
	(MatrixFactory/importFromArray #^"[[D" (doubles-2d xs))))

(defn from-matrix [X]
 (map #(into [] %)
      (.toDoubleArray X)))

(defn fill [v r c]
  (MatrixFactory/fill v (long-array [r c])))

(defn I
"identity matrix"
[& dimensions] (MatrixFactory/eye (long-array dimensions)))

(defn rand-elems
  ([n]
     (let [ra (Random.)]
       (repeatedly n #(.nextDouble ra))))
  ([r c] 
     (repeatedly r #(rand-elems c))))

;;TODO: replace random matrix with.
;; // create a matrix filled with random numbers (Gaussian distribution):
;; Matrix m5 = MatrixFactory.randn(10, 3);
 
;; // create a matrix filled with random numbers (uniform distribution):
;; Matrix m6 = MatrixFactory.rand(5, 5);

(defn fill-rand [r c]
  (matrix (rand-elems r c)))

(defn to-diag [xs]
  (let [n (count xs)
	is (range 0 (+ 1 n))
	zeros (vec (repeat n 0))]
    (map (fn [x i]
	   (assoc zeros i x))
	 xs is)))

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

;;TODO: abstract general pattern for calling these operations on the underlying matrix object
(defn times [#^DenseDoubleMatrix2D A B]
  (if (isa? (class B) DenseDoubleMatrix2D)
  (.mtimes A #^DenseDoubleMatrix2D B)
  (.mtimes A #^Double (double B))))

(defn plus [#^DenseDoubleMatrix2D A B]
  (if (isa? (class B) DenseDoubleMatrix2D)
  (.plus A #^DenseDoubleMatrix2D B)
  (.plus A #^Double (double B))))

(defn trans [#^DenseDoubleMatrix2D A]
  (.transpose A))

(defn svd [#^DenseDoubleMatrix2D A]
  (.svd A))

(defn solve [#^DenseDoubleMatrix2D A #^DenseDoubleMatrix2D B]
  (.solve A B))

(defn inv [#^DenseDoubleMatrix2D A]
  (.inv A))

(defn column-count [#^DenseDoubleMatrix2D A]
  (.getColumnCount A))

(defn row-count [#^DenseDoubleMatrix2D A]
  (.getRowCount A))

(def link-to-matrix Calculation$Ret/LINK)
(def new-matrix Calculation$Ret/NEW)
(def orig-matrix Calculation$Ret/ORIG)

(defn delete-rows
[#^DenseDoubleMatrix2D A rows]
  (.deleteRows A new-matrix (long-array rows)))

(defn delete-columns 
[#^DenseDoubleMatrix2D A columns]
  (.deleteColumns A new-matrix (long-array columns)))
