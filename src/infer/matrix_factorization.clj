(ns infer.matrix-factorization
  (:require [infer.matrix :as m]))

(defprotocol PMaxtrixFactorization
  (factorize [this M k opts] ""))


(defn- lee-seung-X-update [M X Y eps]                       
  (let [numer (m/times (m/trans Y) M)
        denom (m/times X Y (m/trans Y))        
        X-update 
          (m/update-in-place!
            (fn [i j v]
                (* v
                   (/ (m/get-at numer i j)
                      (+ (m/get-at denom i j) eps))))
            (m/copy-matrix X))
        col-sums  
          (->>
            (m/elem-seq X-update)
            (group-by second)
            (reduce
              (fn [res [_ [i j v]]]
                (assoc res j (+ v (get res j 0.0))))
              {}))]
    (m/update-in-place! 
      (fn [i j x]
        (/ x (col-sums j)))
      X-update)))
    
(defn- lee-seung-Y-update [M X Y eps]                       
  (let [numer (m/times (m/trans X) M)
        denom (m/times (m/trans X) X Y)]
    (m/update-in-place!
      (fn [i j v]
        (* v
           (/ (m/get-at numer i j)
              (+ (m/get-at denom i j) eps))))
      (m/copy-matrix Y))))

(defn- lee-seung-nmf [M k 
                      {:keys [num-iters,eps] 
                       :or {num-iters 10 eps 1.0e-9}
                       :as opts }]                      
  (loop [iter 0
         X (m/rand-elems (m/row-count M) k) 
         Y (m/rand-elems k (m/column-count M))]
    (if (= iter num-iters)  [X Y]
      (recur (inc iter)
             (lee-seung-X-update M X Y eps)
             (lee-seung-Y-update M X Y eps)))))

(defrecord LeeSeungFactorization []
  PMaxtrixFactorization
  (factorize [this M k opts] (lee-seung-nmf M k opts)))    