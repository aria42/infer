(defproject infer "1.0-SNAPSHOT"
  :description "inference and machine learning for clojure"
  :dependencies [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.2.0-SNAPSHOT"]
		 [clojure-csv/clojure-csv "1.1.0"]
		 [org.apache.commons/commons-math "2.0"]
                 [ujmp-complete "0.2.4"]
		 [org.apache.mahout/mahout-core "0.3"]
		 [colt/colt "1.2.0"]
		 [incanter/parallelcolt "0.9.4"]]
  :dev-dependencies [[org.clojars.mmcgrana/lein-javac "0.1.0"]
		     [swank-clojure "1.2.0"]
                     [lein-clojars "0.5.0"]])
