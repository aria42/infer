(ns infer.matrix
  (:use clojure.set)
  (:import java.util.Random)
  (:import [org.ujmp.core Matrix
	    MatrixFactory
	    Ops])
  (:import [org.ujmp.core.matrix Matrix2D])
  (:import [org.ujmp.colt
	    ColtSparseDoubleMatrix2D])
;;  (:import [org.apache.mahout.core SparseMatrix])
  (:import [org.ujmp.parallelcolt
	    ParallelColtSparseDoubleMatrix2D])
  (:import [org.ujmp.core.doublematrix
	    DoubleMatrix DenseDoubleMatrix2D
	    DoubleMatrix2D SparseDoubleMatrix2D])
  (:import org.ujmp.core.calculation.Calculation$Ret)
  (:import org.ujmp.core.doublematrix.calculation.general.decomposition.Chol))

(defn leave-out [js ys]
  (difference (into #{} ys) (into #{} js)))

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
	:else
	(MatrixFactory/importFromArray #^"[[D" (doubles-2d xs))))

(defn- sparse-matrix* [xs mk-matrix]
  (let [n-rows (count xs)
	cols (reduce (fn [acc row]
		       (union acc (into #{} (keys row))))
		     #{}
		     xs)
	m (mk-matrix (long-array [n-rows (+ (apply max cols) 1)]))
	row-indices (range 0 (count xs))]
    (dorun
     (map (fn [row r]
	    (dorun (map (fn [[c v]]
			  (.setDouble m v (long-array [r c])))
			row)))
	  xs
	  row-indices))
    m))

(defn sparse-matrix [xs]
 (sparse-matrix* xs #(MatrixFactory/sparse %)))

(defn sparse-colt-matrix [xs]
 (sparse-matrix* xs #(ColtSparseDoubleMatrix2D. %)))

(defn sparse-pcolt-matrix [xs]
 (sparse-matrix* xs #(ParallelColtSparseDoubleMatrix2D. %)))

;; (defn sparse-mahout-matrix [xs]
;;   (let [n-rows (count xs)
;; 	cols (reduce (fn [acc row]
;; 		       (union acc (into #{} (keys row))))
;; 		     #{}
;; 		     xs)
;; 	m (SparseMatrix. (long-array [n-rows (+ (apply max cols) 1)]))
;; 	row-indices (range 0 (count xs))]
;;     (dorun
;;      (map (fn [row r]
;; 	    (dorun (map (fn [[c v]]
;; 			  (.setQuick m r c v)
;; 			row)))
;; 	  xs
;; 	  row-indices))
;;     m)))

(defn from-sparse-matrix [m]
  (map (fn [coord]
	 (conj (into [] (map int coord)) (.getDouble m coord)))
       (.availableCoordinates m)))

(defn from-sparse-2d-matrix [m]
  (let [map-row (fn [[r row]] (into {} (map (fn [[_ b c]] [b c]) row)))]
    (map map-row
	 (group-by first (from-sparse-matrix m)))))

(defn column-matrix [ys]
  (matrix (map vector ys)))

(defn from-matrix [A]
  (map #(into [] %)
       (.toDoubleArray A)))

(defn from-column-matrix [X]
  (flatten (map #(into [] %)
		(.toDoubleArray X))))

(defn fill [v r c]
  (MatrixFactory/fill v (long-array [r c])))

(defn I
"identity matrix"
[& dimensions] (MatrixFactory/eye (long-array dimensions)))

; (defn zero-matrix
;   "Creates a matrix of zeros of dimension r c."
;   [r c]
;   (MatrixFactory/zeros (long-array [r c])))

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

(defn column-count [#^DenseDoubleMatrix2D A]
  (.getColumnCount A))

(defn row-count [#^DenseDoubleMatrix2D A]
  (.getRowCount A))

(defn count-elements [M]
  (* (row-count M)
     (column-count M)))

(defn get-at [#^DoubleMatrix2D m r c]
  (.getDouble m (int r) (int c)))

(defn set-at [#^DoubleMatrix2D m v r c]
 (.setDouble m (double v) (int r) (int c)))

(defn copy-matrix [#^DoubleMatrix2D m]
 (.copy m))

(defn inc-at
  ([m r c] (inc-at m 1 r c))
  ([m by r c]
     (set-at m
	     (+ (get-at m r c) by)
	     r c)
     m))

;;TODO: abstract general pattern for calling these operations on the underlying matrix object
(defn times
  "Performs matrix multiplication of A, an n x k matrix, and B, a k x l matrix,
  and outputs an n x l matrix.  If there are more matrices Ms, continues to
  multiply them all from left to right in the usual fashion."
  ([#^DenseDoubleMatrix2D A B]
  (if (isa? (class B) DenseDoubleMatrix2D)
  (.mtimes A #^DenseDoubleMatrix2D B)
  (.mtimes A #^Double (double B))))
  ([#^DenseDoubleMatrix2D A B & Ms]
     (let [AB (times A B)]
       (if Ms
	 (recur AB (first Ms) (next Ms))
	 AB))))

(defn divide
  ([#^DenseDoubleMatrix2D A B]
  (if (isa? (class B) DenseDoubleMatrix2D)
  (.divide A #^DenseDoubleMatrix2D B)
  (.divide A #^Double (double B))))
  ([#^DenseDoubleMatrix2D A B & Ms]
     (let [AB (divide A B)]
       (if Ms
	 (recur AB (first Ms) (next Ms))
	 AB))))

(defn plus
  "Adds matrices A in B in the usual way, assuming they are of the same size.
  If additional matrices Ms are supplied, adds them all from left to right."
  ([#^DenseDoubleMatrix2D A B]
  (if (isa? (class B) DenseDoubleMatrix2D)
  (.plus A #^DenseDoubleMatrix2D B)
  (.plus A #^Double (double B))))
  ([#^DenseDoubleMatrix2D A B & Ms]
     (let [AB (plus A B)]
       (if Ms
	 (recur AB (first Ms) (next Ms))
	 AB))))

(defn minus
    "Subtracts matrices A in B in the usual way, assuming they are of the same size.
  If additional matrices Ms are supplied, subtracts them all from left to right."
  ([#^DenseDoubleMatrix2D A B]
  (if (isa? (class B) DenseDoubleMatrix2D)
  (.minus A #^DenseDoubleMatrix2D B)
  (.minus A #^Double (double B))))
  ([#^DenseDoubleMatrix2D A B & Ms]
     (let [AB (minus A B)]
       (if Ms
	 (recur AB (first Ms) (next Ms))
	 AB))))

(defn elementwise-plus [e M]
(plus (fill e (row-count M) (column-count M))
       M))

(defn elementwise-minus [e M]
(minus (fill e (row-count M) (column-count M))
       M))

(defn trans [#^DenseDoubleMatrix2D A]
  (.transpose A))

(def rows from-matrix)

(defn columns [A]
  (from-matrix (trans A)))

(defn svd [#^DenseDoubleMatrix2D A]
  (.svd A))

(defn qr [#^DenseDoubleMatrix2D A]
  (.qr A))

(defn chol [#^DenseDoubleMatrix2D A]
  (.chol A))

(defn solve [#^DenseDoubleMatrix2D A #^DenseDoubleMatrix2D B]
  (.solve A B))

(defn inv [#^DenseDoubleMatrix2D A]
  (.inv A))

(def link-to-matrix Calculation$Ret/LINK)
(def new-matrix Calculation$Ret/NEW)
(def orig-matrix Calculation$Ret/ORIG)

(defn delete-rows
[#^DenseDoubleMatrix2D A rows]
  (.deleteRows A new-matrix (long-array rows)))

(defn delete-columns 
[#^DenseDoubleMatrix2D A columns]
  (.deleteColumns A new-matrix (long-array columns)))

(defn select-columns
  [#^DenseDoubleMatrix2D A columns]
  (.selectColumns A new-matrix (long-array columns)))

(defn select-rows
  [#^DenseDoubleMatrix2D A rows]
  (.selectRows A new-matrix (long-array rows)))

(defn column-concat 
[& Ms] (MatrixFactory/horCat (into-array #^Matrix Ms)))

(defn row-concat 
[& Ms] (MatrixFactory/vertCat (into-array #^Matrix Ms)))
