; ======================================================================== 
; lsh.clj - Locality Sensitive Hashing for the Near(est) Neighbors Problem 
; ======================================================================== 
; Currently only supports minhashing, though it is damn easy to switch out 
; the hashing functions, given the lib's modularity.                       
; Version 0.1 - not thread-safe, probably pretty n00b implementation.      
; TODO:                                                                    
; - investigate an approximate minhashing solution.                        
; - proper unit testing.                                                   
; - create a thread-safe version.                                          

(ns projectioneer.lsh
	(:use [clojure.contrib.math :only (floor)])
	(:use [clojure.contrib.seq-utils :only (shuffle)])
	(:use [clojure.set :only (union intersection difference)])
	(:import [java.util Random])
	(:use [projectioneer.random-variate :only (normal-lazy-seq)]))

;; A few helper functions that help me bridge my Clojure ignorance. - - - - ;
;; (defn- interleave-and-pair
;; 	"interleaves each collection of colls, then partitions them into groups of 2 each."
;; 	[col1 col2]
;; 	(apply vector (partition 2 (apply vector (interleave col1 col2)))))

(defn interleave-and-partition
  "Takes a collection of collections, interleaves, then chunks the resulting seq into
  n-sized colls."
  [& coll-of-colls]
  ;(println "Col: " coll-of-colls (count coll-of-colls) "total.")
  (if
   (every? map? coll-of-colls) coll-of-colls
   (partition (count coll-of-colls) (apply interleave coll-of-colls))))

(defn interleave-and-pair
  "interleaves each collection of colls, then partitions them into groups of 2 each."
  [col1 col2]
  (interleave-and-partition col1 col2))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(defn jaccard
  [x1 x2]
  (/ (count (intersection x1 x2)) (count (union x1 x2))))

(defn dot-product
  [x y]
  (apply + (for [[xi yi] (interleave-and-pair x y)] (* xi yi))))
  

;================== Random Components of Hashing Functions =================;

(defn permutation-dictionary
  ""
  [dims]
  (zipmap (shuffle (range dims)) (range dims)))

;===========================================================================;

(defn exact-minhash
  "Originally proposed by Broder (1997), I think."
  [permutation-dict]
  (let [perm permutation-dict]
    (fn [data]
      (let [dict perm]
	(apply min (for [x data] (dict x)))))))


(defn hamming-hash
  "From the original paper on LSH by Indyk and Motwani (1998)."
  [random-index]
  (let [rand-index random-index]
    (fn [data] 
      (let [ri rand-index]
	(data ri)))))

(defn l1-hash
	""
	[random-value width]
	(let [rand-val random-value
	      wi width]
	  (fn [data-coord]
	    (let [rv rand-val
		  w wi]
	      (floor (/ (- data-coord rv) w))  ))))

(defn lp-hash
  "As seen in 'Locality Sensitive Hashing Based on p-Stable Distributions' by Indyk et al. (ACM 2004)."
  [v b r]
  (fn [data]
    (let [rand-v v
	  rand-b b
	   val-r r]
      (floor (/ (+ rand-b (dot-product data rand-v)) val-r)))))

(defn spherical-l2-hash
	"Proposed by Terasawa and Tanaka (2007)")

;; (defn create-hash-ensemble
;;   "Creates a list of hash functions with size number-of-functions, by passing in
;;   the relevant parameters.  Each parameter in params should be a list or vector of
;;   whatever relevant values."
;;   [hash-fcn & param-sets]
;;   (println "psets" param-sets (apply interleave-and-partition param-sets))
;;   (let [pars (apply interleave-and-partition param-sets)]
;;     (for [p pars] (apply hash-fcn p))))

(defn- apply-hash-ensemble
	"Takes a list of minhash functions and data."
	[hash-ensemble data]
	(for [f hash-ensemble] (f data)))

(defn create-lsh-tables 
	"Builds a vector of maps."
	[number-of-hash-functions]
	(dotimes [_ number-of-hash-functions] {}))

(defn data-to-signature
  "Outputs a signature (list) whose vectors represent the hashed values
  of the data."
  [hash-ensemble data]
  (map #(% data) hash-ensemble))

(defn assoc-sig-table
  [tables id-sig]
  (let [[id signature] id-sig]
    (for [[sig table] (interleave-and-pair signature tables)]
      (merge-with  #(union %1 %2) tables {sig #{id}}))))

(defn reduce-signatures-to-table
  "This may as well be delegated to the application, since it
  is obvious.  Perhaps just leave the add-signature-to-buckets
  function in the lib."
  [tables id-signatures]
    (reduce add-signature-to-lsh-tables tables id-signatures))

; Left to implement:
; 1.) data structure inversion (get a map of ids to a set of ids.)
; 2.) ...

; ==================== OLD FUNCTIONALITY ======================== ;

;; (defn assoc-lsh
;; 	"Returns a new list of maps, with id being mapped into the appropriate
;; 	signature bins by its data."
;; 	[ensemble bin-size id data bins]
;; 	(let [signature (partition bin-size (apply-hash-ensemble ensemble data))]
;; 		(for [[sig bin] (interleave-and-pair signature bins)] (merge-with #(union %1 %2) bin {sig #{id}}))))

;; (defn get-lsh
;; 	"Returns a set of ids that match this data point.
;; 	Can be used with non-binned data."
;; 	[ensemble bin-size bins id data-point]
;; 	(let [
;; 		    signature (partition bin-size (apply-hash-ensemble ensemble data-point))
;; 		    sets (for [[sig bin] (interleave-and-pair signature bins)] (get bin sig nil))]
;; 		(difference (reduce union #{} sets) #{id})))


