(ns infer.measures
  (:use clojure.contrib.math)
  (:use clojure.contrib.map-utils)
  (:use clojure.set)
  (:use [incanter.transformations :only [sort-map same-length?]])
  (:use [infer.core :only [tree-comp-each]])
  (:use [infer.probability :only [gt lt binary]])
  (:import [org.apache.commons.math.stat.correlation
	    PearsonsCorrelation Covariance])
  (:import [org.apache.commons.math.stat.descriptive.moment
	    Mean StandardDeviation Variance]))

(defn mean [xs] (.evaluate
		  (Mean.)
		   (double-array xs)))

(defn pow [x y] (Math/pow x y))

(defn correlation [xs ys] (.correlation 
			   (PearsonsCorrelation.)
			   (double-array xs)
			   (double-array ys)))

(defn covariance [xs ys] (.covariance
			  (Covariance.)
			  (double-array xs)
			  (double-array ys)))

(defn variance [xs] (.evaluate
			(Variance.)
			(double-array xs)))

(defn st-dev [xs]
  (.evaluate
   (StandardDeviation.)
   (double-array xs)))

;;TODO: finish gamma, kendall's-w and 

;;look for old impl, ore reimpliment.  http://en.wikipedia.org/wiki/Mahalanobis_distance

;;TODO mote this to a new task in tracker: nonparametric stuff such as: http://en.wikipedia.org/wiki/Median_absolute_deviation

;;smoothing.
;;http://en.wikipedia.org/wiki/Smoothing
;;http://en.wikipedia.org/wiki/Kernel_smoother
;;http://en.wikipedia.org/wiki/Kernel_density_estimation
;;http://en.wikipedia.org/wiki/Local_regression
;;http://en.wikipedia.org/wiki/Kernel_regression

;;DEPENDENCE
;; http://en.wikipedia.org/wiki/Association_(statistics)
;; http://en.wikipedia.org/wiki/Independence_(probability_theory)
;; http://en.wikipedia.org/wiki/Goodman_and_Kruskal%27s_lambda
;; http://en.wikipedia.org/wiki/Category:Statistical_dependence
;; http://en.wikipedia.org/wiki/Tetrachoric_correlation_coefficient
;; http://en.wikipedia.org/wiki/Polychoric_correlation
;;TODO:  http://en.wikipedia.org/wiki/Multiple_correlation
;;TODO: further work in mutivariate dependence
;;http://en.wikipedia.org/wiki/Concordance_correlation_coefficient

;;TODO: more research on spellchecking, string similarity
;;http://en.wikipedia.org/wiki/Approximate_string_matching
;;http://en.wikipedia.org/wiki/Longest_common_subsequence_problem

(defn within 
"
y is within z of x in metric space.  
"
[z x y]
 (< (abs (- x y)) z))

(defn square-devs-from-mean
"takes either a sample or a sample and a precalculated mean.

returns the squares of the difference between each observation and the sample mean."
  ([x]
     (square-devs-from-mean x (mean x)))

  ([x m]
     (map 
      #(pow 
        (- % m) 2)
      x)))

(defn sum-of-square-devs-from-mean 
"takes either a sample or a sample and a precalculated mean.

returns the sum of the squares of the difference between each observation and the sample mean."
([x]
(sum-of-square-devs-from-mean x (mean x)))

([x m]
   (apply + (square-devs-from-mean x m))))

(defn odds-ratio
"
http://en.wikipedia.org/wiki/Odds_ratio

Definition in terms of group-wise odds

The odds ratio is the ratio of the odds of an event occurring in one group to the odds of it occurring in another group, or to a sample-based estimate of that ratio.


Suppose that in a sample of 100 men, 90 have drunk wine in the previous week, while in a sample of 100 women only 20 have drunk wine in the same period. The odds of a man drinking wine are 90 to 10, or 9:1, while the odds of a woman drinking wine are only 20 to 80, or 1:4 = 0.25:1. The odds ratio is thus 9/0.25, or 36, showing that men are much more likely to drink wine than women. 

Relation to statistical independence

If X and Y are independent, their joint probabilities can be expressed in terms of their marginal probabilities.  In this case, the odds ratio equals one, and conversely the odds ratio can only equal one if the joint probabilities can be factored in this way. Thus the odds ratio equals one if and only if X and Y are independent.

"
[p1 p2]
(/ (* p1 (- 1 p2))
   (* p2 (- 1 p1))))

(defn correlation-ratio 
"
http://en.wikipedia.org/wiki/Correlation_ratio

In statistics, the correlation ratio is a measure of the relationship between the statistical dispersion within individual categories and the dispersion across the whole population or sample. i.e. the weighted variance of the category means divided by the variance of all samples.

Example

Suppose there is a distribution of test scores in three topics (categories):

    * Algebra: 45, 70, 29, 15 and 21 (5 scores)
    * Geometry: 40, 20, 30 and 42 (4 scores)
    * Statistics: 65, 95, 80, 70, 85 and 73 (6 scores).

Then the subject averages are 36, 33 and 78, with an overall average of 52.

The sums of squares of the differences from the subject averages are 1952 for Algebra, 308 for Geometry and 600 for Statistics, adding to 2860, while the overall sum of squares of the differences from the overall average is 9640. The difference between these of 6780 is also the weighted sum of the square of the differences between the subject averages and the overall average:

    5(36 − 52)2 + 4(33 − 52)2 + 6(78 − 52)2 = 6780

This gives

    eta^2 =6780/9640=0.7033

suggesting that most of the overall dispersion is a result of differences between topics, rather than within topics. Taking the square root

    eta = sqrt 6780/9640=0.8386

Observe that for η = 1 the overall sample dispersion is purely due to dispersion among the categories and not at all due to dispersion within the individual categories. For a quick comprehension simply imagine all Algebra, Geometry, and Statistics scores being the same respectively, e.g. 5 times 36, 4 times 33, 6 times 78.
"
[& xs]
 (let [sos (map sum-of-square-devs-from-mean xs)
       all (apply concat xs)
       overall-sos (sum-of-square-devs-from-mean all)]
(sqrt 
 (/ (- overall-sos (apply + sos))
   overall-sos))))

(defn correlation-linearity-test
"
http://en.wikipedia.org/wiki/Correlation_ratio

It is worth noting that if the relationship between values of  and values of overline y_x is linear (which is certainly true when there are only two possibilities for x) this will give the same result as the square of the correlation coefficient, otherwise the correlation ratio will be larger in magnitude. It can therefore be used for judging non-linear relationships.
"
[a b]
(- (correlation-ratio a b)
   (correlation a b)))

(defn rank-index
"
given a seq, returns a map where the keys are the values of the seq and the values are the positional rank of each member o the seq.
"
[x]
(zipmap (sort x) (range 1 (+ 1 (count x)))))

(defn spearmans-rho
"
http://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient

In statistics, Spearman's rank correlation coefficient or Spearman's rho, is a non-parametric measure of correlation – that is, it assesses how well an arbitrary monotonic function could describe the relationship between two variables, without making any other assumptions about the particular nature of the relationship between the variables. Certain other measures of correlation are parametric in the sense of being based on possible relationships of a parameterised form, such as a linear relationship.
"
[a b]
(let [_ (assert (same-length? a b))
      n (count a)
      arank (rank-index a)
      brank (rank-index b)
      dsos (apply 
            + (map (fn [x y] (pow
                      (- (arank x) (brank y)) 
                          2))
           a b))]
  (- 1 (/ (* 6 dsos) 
          (* n (- (pow n 2) 1))))))

(defn kendalls-tau
"
http://en.wikipedia.org/wiki/Kendall_tau_rank_correlation_coefficient
http://www.statsdirect.com/help/nonparametric_methods/kend.htm
http://mail.scipy.org/pipermail/scipy-dev/2009-March/011589.html
best explanation and example is in \"cluster analysis for researchers\" page 165.
http://www.amazon.com/Cluster-Analysis-Researchers-Charles-Romesburg/dp/1411606175
"
[a b]
(let [_ (assert (same-length? a b))
      n (count a)
      ranked (reverse (sort-map (zipmap a b)))
      ;;dcd is the meat of the calculation, the difference between the doncordant and discordant pairs
      dcd (second
           (reduce
           (fn [[vals total] [k v]]
             (let [diff (- (count (filter (gt v) vals))
                           (count (filter (lt v) vals)))]
               [(conj vals v) (+ total diff)]))
           [[] 0]
           ranked))]
(/ (* 2 dcd)
   (* n (- n 1)))))

(defn pairs 
"returns unique pairs of a and b where members of a and b can not be paired with the correspoding slot in the other list."
[a b]
((fn combine [combos ra rb]
    (let [heada (first ra)
          level-combos (for [bx (rest rb)]
                         [heada bx])
          all-combos (concat combos level-combos)]
      (if (= 0 (count (rest ra)))
        all-combos
        (combine all-combos (rest ra) (rest rb))))) [] a b))

(defn pairings 
  "confusing ass name."
  [a b]
  (let [tuples (zipmap a b)]
    (pairs tuples tuples)))

(defn concordant?
  [[[a1 b1][a2 b2]]]
  (= (compare a1 a2)
     (compare b1 b2)))

(def discordant? (comp not concordant?))

(defn discordant-pairs
"http://en.wikipedia.org/wiki/Discordant_pairs"
[a b]
 (let [tuples (zipmap a b)
       ps (pairs tuples tuples)]
   (count (filter discordant? ps))))

(def kendalls-tau-distance discordant-pairs)

;;TODO: factor out duplication between the distance metric and regular kendall's tau
(defn normalized-kendall-tau-distance
"
http://en.wikipedia.org/wiki/Kendall_tau_distance

Kendall tau distance is the total number of discordant pairs.
"
[a b]
(let [n (count a)
      discords (discordant-pairs a b)]
(/ (* 2 discords)
   (* n (- n 1)))))


(defn gamma-coefficient
"
http://www.statsdirect.com/help/nonparametric_methods/kend.htm

The gamma coefficient is given as a measure of association that is highly resistant to tied data (Goodman and Kruskal, 1963):
"
[]
)

(defn kendalls-w
"
http://en.wikipedia.org/wiki/Kendall%27s_W
http://faculty.chass.ncsu.edu/garson/PA765/friedman.htm

Suppose that object i is given the rank ri,j by judge number j, where there are in total n objects and m judges. Then the total rank given to object i is

    Ri = sum Rij

and the mean value of these total ranks is

    Rbar = 1/2 m (n + 1)

The sum of squared deviations, S, is defined as

    S=sum1-n (Ri - Rbar)

and then Kendall's W is defined as[1]

    W= 12S / m^2(n^3-n)

If the test statistic W is 1, then all the survey respondents have been unanimous, and each respondent has assigned the same order to the list of concerns. If W is 0, then there is no overall trend of agreement among the respondents, and their responses may be regarded as essentially random. Intermediate values of W indicate a greater or lesser degree of unanimity among the various responses.

Legendre[2] discusses a variant of the W statistic which accommodates ties in the rankings and also describes methods of making significance tests based on W.

 [{:observation [1 2 3]} {} ... {}] -> W
"
[])




(defn sum-variance-test 
"the variance of the sum of n independent variables is equal to the sum of their variances.

 (variance-independence-test [[1 2 3 4] [1 2 3 4]]) -> 5/2
"
[vs]
 (- (variance (apply map + vs))
    (apply + (map variance vs))))

;;TODO: don't impliment until fully understanding whether thse are just f-divergences.
(defn product-marginal-test 
"the joint PMF of independent variables is equal to the product of their marginal PMFs."
[j]
)


;;TODO: seems very useful for clustering: http://en.wikipedia.org/wiki/Mahalanobis_distance and http://en.wikipedia.org/wiki/Partial_leverage
;;TODO: add http://en.wikipedia.org/wiki/Jaro-Winkler
;;TODO: add graphical approaches to similarity: http://en.wikipedia.org/wiki/SimRank
;;TODO: string similarity measures: http://en.wikipedia.org/wiki/String_metric

(defn minkowski-distance
"http://en.wikipedia.org/wiki/Minkowski_distance
http://en.wikipedia.org/wiki/Lp_space

The Minkowski distance is a metric on Euclidean space which can be considered as a generalization of both the Euclidean distance and the Manhattan distance.

Minkowski distance is typically used with p being 1 or 2. The latter is the Euclidean distance, while the former is sometimes known as the Manhattan distance.

In the limiting case of p reaching infinity we obtain the Chebyshev distance."
 [a b p]
(let [_ (assert (same-length? a b))]

 (pow
  (apply
   tree-comp-each
   + 
  (fn [[x y]] 
    (pow 
     (abs 
      (- x y)) 
     p))
  (map vector a b))
  (/ 1 p))))

(defn euclidean-distance
"http://en.wikipedia.org/wiki/Euclidean_distance

the Euclidean distance or Euclidean metric is the ordinary distance between two points that one would measure with a ruler, and is given by the Pythagorean formula. By using this formula as distance, Euclidean space (or even any inner product space) becomes a metric space. The associated norm is called the Euclidean norm. Older literature refers to the metric as Pythagorean metric."
 [a b]
(minkowski-distance a b 2))

(defn chebyshev-distance
"In the limiting case of Lp reaching infinity we obtain the Chebyshev distance."
[a b]
(let [_ (assert (same-length? a b))]
  (apply
   tree-comp-each
   max 
  (fn [[x y]] (- x y))
  (map vector a b))))

(defn manhattan-distance
"http://en.wikipedia.org/wiki/Manhattan_distance

usual metric of Euclidean geometry is replaced by a new metric in which the distance between two points is the sum of the (absolute) differences of their coordinates. The taxicab metric is also known as rectilinear distance, L1 distance or l1 norm (see Lp space), city block distance, Manhattan distance, or Manhattan length"
 [a b]
(minkowski-distance a b 1))

(defn cosine-similarity
"
http://en.wikipedia.org/wiki/Cosine_similarity
http://www.appliedsoftwaredesign.com/cosineSimilarityCalculator.php

The Cosine Similarity of two vectors a and b is the ratio: a dot b / ||a|| ||b||

Let d1 = {2 4 3 1 6}
Let d2 = {3 5 1 2 5}

Cosine Similarity (d1, d2) =  dot(d1, d2) / ||d1|| ||d2||

dot(d1, d2) = (2)*(3) + (4)*(5) + (3)*(1) + (1)*(2) + (6)*(5) = 61

||d1|| = sqrt((2)^2 + (4)^2 + (3)^2 + (1)^2 + (6)^2) = 8.12403840464

||d2|| = sqrt((3)^2 + (5)^2 + (1)^2 + (2)^2 + (5)^2) = 8

Cosine Similarity (d1, d2) = 61 / (8.12403840464) * (8)
                           = 61 / 64.9923072371
                           = 0.938572618717
"
[a b]
(let [counts
(apply merge-with +
(map 
 (fn [[x y]]
   {:dot (* x y)
    :a (pow x 2)
    :b (pow y 2)})
 (map vector a b)))]
(/ (:dot counts)
   (* (sqrt (:a counts))
      (sqrt (:a counts))))))

(defn tanimoto-coefficient
"

http://en.wikipedia.org/wiki/Jaccard_index

The cosine similarity metric may be extended such that it yields the Jaccard coefficient in the case of binary attributes. This is the Tanimoto coefficient. "
[a b]
(let [counts
(apply merge-with +
(map 
 (fn [[x y]]
   {:dot (* x y)
    :a (pow x 2)
    :b (pow y 2)})
 (map vector a b)))]
(/ (:dot counts)
   (- 
    (+ (:a counts)
       (:a counts))
    (:dot counts)))))

(defn jaccard-index 
"
http://en.wikipedia.org/wiki/Jaccard_index

The Jaccard index, also known as the Jaccard similarity coefficient (originally coined coefficient de communauté by Paul Jaccard), is a statistic used for comparing the similarity and diversity of sample sets.

The Jaccard coefficient measures similarity between sample sets, and is defined as the size of the intersection divided by the size of the union of the sample sets."
[a b]
 (/ (count (intersection a b))
    (count (union a b))))

(defn jaccard-distance
"
http://en.wikipedia.org/wiki/Jaccard_index

The Jaccard distance, which measures dissimilarity between sample sets, is complementary to the Jaccard coefficient and is obtained by subtracting the Jaccard coefficient from 1, or, equivalently, by dividing the difference of the sizes of the union and the intersection of two sets by the size of the union."
[a b]
(- 1 (jaccard-index a b)))

(defn dice-coefficient
"
http://en.wikipedia.org/wiki/Dice%27s_coefficient
Dice's coefficient (also known as the Dice coefficient) is a similarity measure related to the Jaccard index.
"
[a b]
(let [an (count a)
      bn (count b)
      cn (count (intersection a b))]
(/ (* 2 cn)
   (+ an bn))))

(defn n-grams
"returns a set of the unique n-grams in a string.

this is using actual sets here, discards dupicate n-grams?
"
 [n s] 
(into 
 #{}
 (map 
  #(apply str %) 
  (partition n 1 s))))

(defn bigrams [s] (n-grams 2 s))

(defn dice-coefficient-str
"
http://en.wikipedia.org/wiki/Dice%27s_coefficient

When taken as a string similarity measure, the coefficient may be calculated for two strings, x and y using bigrams.  here nt is the number of character bigrams found in both strings, nx is the number of bigrams in string x and ny is the number of bigrams in string y. For example, to calculate the similarity between:

    night
    nacht

We would find the set of bigrams in each word:

    {ni,ig,gh,ht}
    {na,ac,ch,ht}

Each set has four elements, and the intersection of these two sets has only one element: ht.

Plugging this into the formula, we calculate, s = (2 · 1) / (4 + 4) = 0.25.
"
[a b]
(dice-coefficient 
 (bigrams a)
 (bigrams b)))

(defn hamming-distance
"http://en.wikipedia.org/wiki/Hamming_distance

In information theory, the Hamming distance between two strings of equal length is the number of positions at which the corresponding symbols are different. Put another way, it measures the minimum number of substitutions required to change one string into the other, or the number of errors that transformed one string into the other."
[a b]
(if (and (integer? a) (integer? b))
  (hamming-distance (str a) (str b))
(let [_ (assert (same-length? a b))]
(apply
 tree-comp-each 
  + 
  #(binary (not (apply = %)))
  (map vector a b)))))

(defn lee-distance
"http://en.wikipedia.org/wiki/Lee_distance

In coding theory, the Lee distance is a distance between two strings x1x2...xn and y1y2...yn of equal length n over the q-ary alphabet {0,1,…,q-1} of size q >= 2. It is metric.

If q = 2 or q = 3 the Lee distance coincides with the Hamming distance.

The metric space induced by the Lee distance is a discrete analog of the elliptic space.
"
[a b q]
(if (and (integer? a) (integer? b))
  (lee-distance (str a) (str b) q)
(let [_ (assert (same-length? a b))]
(apply
 tree-comp-each 
  + 
  (fn [x]
    (let [diff (abs (apply - (map int x)))]
      (min diff (- q diff))))
  (map vector a b)))))

(defn sorensen-index
"
http://en.wikipedia.org/wiki/S%C3%B8rensen_similarity_index#cite_note-4

The Sørensen index, also known as Sørensen’s similarity coefficient, is a statistic used for comparing the similarity of two samples. where A and B are the species numbers in samples A and B, respectively, and C is the number of species shared by the two samples. 

 The Sørensen index is identical to Dice's coefficient which is always in [0, 1] range. Sørensen index used as a distance measure, 1 − QS, is identical to Hellinger distance and Bray–Curtis dissimilarity.

The Sørensen coefficient is mainly useful for ecological community data (e.g. Looman & Campbell, 1960[3]). Justification for its use is primarily empirical rather than theoretical (although it can be justified theoretically as the intersection of two fuzzy sets[4]). As compared to Euclidean distance, Sørensen distance retains sensitivity in more heterogeneous data sets and gives less weight to outliers

This function assumes you pass in a and b as sets.

The sorensen index extended to abundance instead of incidence of species is called the Czekanowski index."
[a b]
(dice-coefficient a b))

(defn levenshtein-distance
"
http://en.wikipedia.org/wiki/Levenshtein_distance

internal representation is a table d with m+1 rows and n+1 columns

where m is the length of a and m is the length of b.

In information theory and computer science, the Levenshtein distance is a metric for measuring the amount of difference between two sequences (i.e., the so called edit distance). The Levenshtein distance between two strings is given by the minimum number of operations needed to transform one string into the other, where an operation is an insertion, deletion, or substitution of a single character.

For example, the Levenshtein distance between \"kitten\" and \"sitting\" is 3, since the following three edits change one into the other, and there is no way to do it with fewer than three edits:

   1. kitten → sitten (substitution of 's' for 'k')
   2. sitten → sittin (substitution of 'i' for 'e')
   3. sittin → sitting (insert 'g' at the end).

The Levenshtein distance has several simple upper and lower bounds that are useful in applications which compute many of them and compare them. These include:

    * It is always at least the difference of the sizes of the two strings.
    * It is at most the length of the longer string.
    * It is zero if and only if the strings are identical.
    * If the strings are the same size, the Hamming distance is an upper bound on the Levenshtein distance.

"
  [a b]
  (let [m (count a)
        n (count b)
        init (apply deep-merge-with (fn [a b] b)
                    (concat 
                     ;;deletion
                     (for [i (range 0 (+ 1 m))]
                       {i {0 i}})
                     ;;insertion
                     (for [j (range 0 (+ 1 n))]
                       {0 {j j}})))
        table (reduce
               (fn [d [i j]]
                 (deep-merge-with 
                  (fn [a b] b) 
                  d 
                  {i {j (if (= (nth a (- i 1))
                               (nth b (- j 1)))
                          ((d (- i 1)) (- j 1))
                          (min 
                           (+ ((d (- i 1)) 
                               j) 1) ;;deletion
                           (+ ((d i) 
                               (- j 1)) 1) ;;insertion
                           (+ ((d (- i 1)) 
                               (- j 1)) 1))) ;;substitution))
                      }}))
               init
               (for [j (range 1 (+ 1 n))
                     i (range 1 (+ 1 m))] [i j]))]

    ((table m) n)))


(defn damerau-levenshtein-distance
  [a b]
  (let [m (count a)
        n (count b)
        init (apply deep-merge-with (fn [a b] b)
                    (concat 
                     ;;deletion
                     (for [i (range 0 (+ 1 m))]
                       {i {0 i}})
                     ;;insertion
                     (for [j (range 0 (+ 1 n))]
                       {0 {j j}})))
        table (reduce
               (fn [d [i j]]
                 (deep-merge-with 
                  (fn [a b] b) 
                  d 
                  (let [cost (binary (not (= (nth a (- i 1))
                                          (nth b (- j 1)))))
                        x
                          (min 
                           (+ ((d (- i 1)) 
                               j) 1) ;;deletion
                           (+ ((d i) 
                               (- j 1)) 1) ;;insertion
                           (+ ((d (- i 1)) 
                               (- j 1)) cost)) ;;substitution))
                      
                        val (if (and (> i 1)
                               (> j 1)
                               (= (nth a (- i 1))
                                  (nth b (- j 2)))
                               (= (nth a (- i 2))
                                  (nth b (- j 1))))
                        (min x (+ ((d (- i 2)) 
                                   (- j 2)) ;;transposition
                                  cost))
                        x)]
                    {i {j val}})))
               init
               (for [j (range 1 (+ 1 n))
                     i (range 1 (+ 1 m))] [i j]))]

    ((table m) n)))