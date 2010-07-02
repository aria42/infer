(ns infer.random-variate
	(:use [clojure.contrib.math :only (expt sqrt)])
	(:use [clojure.contrib.generic.math-functions :only (tan log cos sin)]))

(defn exp-rv
	"Simulate an exponential distribution with
	rate lambda."
	([] (exp-rv 1))
	([lambda]
	(let [U (rand)]
		(/ (log (- 1 U)) (- 0 lambda)))))

(defn weibull-rv [a]
	(let [U (rand)]
		(expt (log (/ 1 U)) a)))

(defn cauchy-rv [sigma]
	(let [U (rand)]
		(* sigma (tan (* Math/PI U)))))

(defn triangular-rv [a]
	"Generate a random variable from triangular distribution
	from 0 to a."
	(let [U (rand)]
		(* a (- 1 (sqrt U)))))

(defn pareto-rv [a b]
	"Generate a random variable from a pareto distribution
	with parameters a, b."
	(let [U (rand)]
		(/ b (expt U (/ 1 a)))))

(defn box-muller
	"Generate two uncorrelated unit-Normal random variables."
	[]
	(let [U1 (rand)
		    U2 (rand)
		    R1 (* (sqrt (* (- 0 2) (log U1))) (cos (* 2 Math/PI U2)))
		    R2 (* (sqrt (* (- 0 2) (log U1))) (sin (* 2 Math/PI U2)))]
		[R1 R2]))

(defn normal-lazy-seq
	"Generate a lazy sequence of unit normal random variables."
	[]
	(let [bm (box-muller)]
	(lazy-seq (concat bm (normal-lazy-seq)))))