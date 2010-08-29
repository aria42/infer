(ns infer.matrix-factorization
  {:doc "Non-negative matrix factorization routines. Currently
        only the classical Lee-Seung multiplicative update is 
        available. Should respect sparsity of matrix if present.
        
        TODO: Alternating Least Squares"
  :author "Aria Haghighi (aria@cs.umass.edu)"}  
  (:require [infer.matrix :as m]))


(defprotocol PMaxtrixFactorization
  (internal-factorize [this M k] 
    "Approximate M (m x n) with XY, where X is (m x k) and Y is (n x k)
     Should return [X Y] and (infer.matrix/times X Y) gives the approximation"))


(defn frobenius-error 
  "returns frobenius norm error of approximation e.g., || M - XY ||_2"
  [M X Y]
  (m/frobenius-norm (m/minus M (m/times X Y))))


;; -------------------------------------------------------
;;  Lee Seung MultiplicativeUpdate
;; -------------------------------------------------------

(defn- column-sums 
  "returns map of column-index to sum of column elements."
  [X]
  (->>
    (m/elem-seq X)
    (reduce
      (fn [res [i j v]]
        (assoc! res j (+ v (get res j 0.0))))
      (transient {}))
    persistent!))


(defn- normalize-columns!
  "updates matrix so each column sums to 1"
  [X]
  (let [col-sums (column-sums X) ]
    (m/update-matrix! 
      (fn [i j v]
        (let [s (get col-sums j 0.0)]
          (if (> s 0.0) (/ v s) v)))
      X)))


(defn- lee-seung-X-update [M X Y eps]
  (let [numer (m/times  M (m/trans Y))
        denom (m/times X Y (m/trans Y))
        update-fn 
          (fn [i j v]
              (* v
                 (/ (m/get-at numer i j)
                    (+ (m/get-at denom i j) eps))))]
    (->> X
        (m/update-matrix  update-fn)       
        normalize-columns!)))
    


(defn- lee-seung-Y-update [M X Y eps]
  (let [numer (m/times (m/trans X) M)
        denom (m/times (m/trans X) X Y)
        update-fn
          (fn [i j v]
            (* v
               (/ (m/get-at numer i j)
                  (+ (m/get-at denom i j) eps))))]
    (m/update-matrix update-fn Y)))


(defn lee-seung-factorize [M k
                    {:keys [num-iters,eps] 
                     :or {num-iters 1000 eps 1.0e-10}
                     :as opts }]              
  (loop [iter 0
         X (normalize-columns! (m/fill-rand (m/row-count M) k)) 
         Y (normalize-columns! (m/fill-rand k (m/column-count M)))]
    (if (= iter num-iters)  [X Y]
      (recur (inc iter)
             (lee-seung-X-update M X Y eps)
             (lee-seung-Y-update M X Y eps)))))


(defrecord LeeSeungFactorization [opts]
  PMaxtrixFactorization
  (internal-factorize [this M k] (lee-seung-factorize M k opts)))    


(defn factorize 
  "Approximately factorizes M into [X,Y] with some implementation. This
   is where to go if you don't want to think about any details. The only
   parmaeter k is which controls how big the approximation is. 
   k should be smaller than the maximum of the rows or column of M"
  [M k]  
  (internal-factorize (LeeSeungFactorization. {:num-iters 100}) M k))
