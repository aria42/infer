(ns infer.lsh-test
  (:use infer.lsh)
  (:use clojure.test))

(def v1 [5 5 5 5])
(def permutation-dict-1 {3 0, 2 1, 1 2, 0 3})
(def permutation-dict-2 {3 2, 2 1, 1 0, 0 3})
(def sparse-pt-1 #{1 3})
(def sparse-pt-2 #{3 2})
(def hamming-pt-1 [0 0 0 1 0 0 0 0 0])
(def hamming-pt-2 [1 2 10 4 5 6 7 8 9 20])
(def lp-pt-1 [1 1 1 1 1])
(def lp-pt-2 [1 1 0 0 0])

(def lsh-table [{} {}])
(def lsh-hamming-hash-fcns
  [(hamming-hash 1) (hamming-hash 2)])
(def hashed-hamming 
  (map #(% hamming-pt-2) lsh-hamming-hash-fcns))
  
(deftest basic-dot-product
  (is (= 100 (dot-product v1 v1))))

(deftest basic-exact-minhash
  (is (= 0 ((exact-minhash permutation-dict-1) sparse-pt-1)))
  (is (= [0 1]
       (map
         #(% sparse-pt-2)
         (map
           exact-minhash 
           [permutation-dict-1 
           permutation-dict-2])))))

(deftest basic-hamming-hash
  (is (= 1 ((hamming-hash 3) hamming-pt-1)))
  (is (= [1 10]
    (map #(% hamming-pt-2)
         (map hamming-hash [0 2])))))

(deftest basic-lp-hash
  (is (= 1 ((lp-hash lp-pt-1 1 5) lp-pt-1)))
  (is (= [1 0]
      (map #(% lp-pt-1)
       (map lp-hash
         [lp-pt-1 lp-pt-2]
         [1  1]
         [5 5])))))

(deftest add-to-lsh
  (is (= [{[2] 1} {[10] 1}] (assoc-lsh lsh-table hashed-hamming 1))))