# Infer
## Inference and Machine Learning in Clojure

Infer is a library for machine learning and statistical inference, designed to be used in real production systems.

Infer is written in [Clojure](http://clojure.org/) and leverages many JVM packages.

You can read about some of the performance testing that went into the foundation here:

+ [Fast Statistics on the JVM](http://measuringmeasures.com/blog/2010/4/17/numerics-benchmarking-fast-statistics-on-the-jvm.html)
+ [Linear Algebra Benchmarks](http://measuringmeasures.com/blog/2010/3/28/matrix-benchmarks-fast-linear-algebra-on-the-jvm.html)
+ [Fast Clojure Vectors and Multidimensional Arrays](http://measuringmeasures.com/blog/2010/3/27/fast-clojure-vectors-and-multidimensional-arrays.html)

Infer does not aim to replace R, or be an R for the JVM, or be a NumPy written in Clojure.

Infer seeks to be a new kind of data tool.

1. __Infer was initially extracted from production use__.  It follows the philosophy that you should be able to deploy your research code, not be forced to rewrite it in C++ or java to deploy to production.

2. __Infer was initially extracted from use with [Hadoop](http://hadoop.apache.org/)__.  It follows the philosophy that you should be able to scale up your research computations with minimal effort, deploy clusters from your local interactive environment, and run your code in a cluster almost as easily as your run it on your local machine.

3. __Infer was initially extracted from real life research and deployment of machine learning systems__.  It follows the philosophy that abstractions should be composable so that you can try out different algorithms, and different combinations of measures, models, and learning algorithms. Most systems, like R, Weka, Lingpipe, or Matlab, for example, have all the methods coded up in silos, and not meant to be used composably.  Want to learn tree models using different loss functions, different measures of statistical diveregence, or a different peanalized learning algorithm?  Traditional libraries say too bad, Infer says, rock it out.


## What is in Infer?

Infer is broad and covers, or intends to cover, most of what you'll find in statistical and machine learning packages (and many techniques that are not).

- *matrices*: wraps the [UJMP](http://www.ujmp.org/) matrix package, which provides an expressive and expansive set of efficient matrix operations.  (examples)

- *probabilities*:  a language for dealing with probabilities.  composable into constructs like graphical models, tress, classifiers, etc.

- *measures*: a vast library of measure functions, statistics, and the like.  Everything you want for measuring things in your learners.  We also have man information-theoretic measures in *information-theory*.

- *linear models*: ols, gls, glms, and penalized / regularized regression methods - L1, L2, and combinations thereof. (examples)

- *neighbor methods*: lsh methods, nearest neighbor queries, kernel methods

- *learning*:  convex optimization, regularized learning, subset selection

- *cross validation*:  k-fold, leave-one-out, etc.

- generalized classification & regression (stuff that currently lives in classification)

- *features*:  easy to deal with feature representations as a matrix, a Clojure vector of vectors, or nested maps, and transforming between the options.  easy dealing with continuous or discrete, or discretizing continuous variables (currently in *classification*).  merging equivalence classes.

- Copyright (c) Bradford Cross and Hamilton Ulmer released under the MIT License (http://www.opensource.org/licenses/mit-license.php).