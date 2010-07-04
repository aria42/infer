(ns infer.lsh
	(:use [clojure.contrib.math :only (floor)])
	(:use [clojure.set :only (union intersection difference)])
	(:import [java.util Random])
	(:use [infer.random-variate :only (random-normal)]))

(defn dot-product
  "TODO: get rid of this implementation"
  [x y]
  (apply + (map * x y)))

(defn permutation-dictionary
  "Creates a map of values randomly mapped.
  TODO: rewrite."
  [dims]
  (zipmap (shuffle (range dims)) (range dims)))

(defn exact-minhash
  "Originally proposed by Broder (1997), I think."
  [permutation-dict]
    (fn [data]
		(apply min (map permutation-dict data))))

(defn hamming-hash
  "From the original paper on LSH by Indyk and Motwani (1998)."
  [random-index]
    (fn [data] 
      (data random-index)))

(defn l1-hash
	"TODO: reimplement"
	[random-value width]
	  (fn [data-coord]
	      (floor (/ (- data-coord random-value) width)))) 

(defn lp-hash
  "As seen in 'Locality Sensitive Hashing Based on p-Stable Distributions' by Indyk et al. (ACM 2004).
  TODO: integrate UJMP stuff and dot product."
  [v b r]
  (fn [data]
      (floor (/ (+ b (dot-product data v)) r))))

(defn spherical-l2-hash
	"Proposed by Terasawa and Tanaka (2007)")

(defn- apply-hash-ensemble
	"Takes a list of minhash functions and data."
	[hash-ensemble data]
	(for [f hash-ensemble] (f data)))

(defn create-lsh-tables 
	"Builds a vector of maps."
	[number-of-hash-functions]
	(dotimes [_ number-of-hash-functions] {}))

(defn map-hash
  "Outputs a signature (list) whose vectors represent the hashed values
  of the data."
  [hash-ensemble data]
  (map #(% data) hash-ensemble))

(defn assoc-lsh
  ^{
    :arglists '([table sig id] [table sig id & sig-ids])
    :doc "Mimics the core language's assoc function. 
    maps id to multiple maps, where sig acts like a key."
  }
  ([table sig id]
    (map 
      #(merge-with union %1 {%2 #{id}})
      table 
      (partition (/ (count sig) (count table)) sig)))
  ([table sig id & sig-ids]
    (let [ret 
            (map 
              #(merge-with union %1 {%2 #{id}})
              table
              (partition (/ (count sig) (count table)) sig))]
      (if sig-ids
        (recur ret (first sig-ids) (second sig-ids) (nnext sig-ids))
        ret))))

(defn merge-tables
  "Merges together many tables of hashed indices into one table."
  [& tables]
  (let [lsh-merge
	(fn [m1 m2] (map #(merge-with union %1 %2) m1 m2))]
  (reduce lsh-merge tables)))

