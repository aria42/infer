(ns infer.streaming-test
  (:use infer.streaming
	clojure.test)
  (:import infer.RollingMean
	   infer.AccumulatingMean
	   infer.RollingSum
	   infer.RollingQueue
	   infer.AccumulatingSum))

(deftest enqueueing
  (let [q (RollingQueue. 3)]
  (is (Double/isNaN (.enqueue q 15)))
  (is (Double/isNaN (.enqueue q 3)))
  (is (Double/isNaN (.enqueue q 17)))
  (is (= 15 (.enqueue q 2)))
  (is (= 3 (.enqueue q 3)))
  (is (= 17 (.enqueue q 4)))))

(deftest enqueueing-state
  (let [q (RollingQueue. 3)
	_ (.enqueue q 15)
	_  (.enqueue q 3)]

  (is (= 15 (.getAtLookback q 1)))
  (is (= 15 (.getAtIndex q 0)))
  (is (= 2 (.getLength q)))

  (let [_ (.enqueue q 17)]

  (is (= 3 (.getAtLookback q 1)))
  (is (= 17 (.getAtLookback q 0)))
  (is (= 15 (.getAtLookback q 2)))
  (is (= 3 (.getAtIndex q 1)))
  (is (= 3 (.getLength q)))

  (let [_ (.enqueue q 1)]

  (is (= 1 (.getAtLookback q 0)))
  (is (= 17 (.getAtLookback q 1)))
  (is (= 3 (.getAtLookback q 2)))
  (is (= 1 (.getAtIndex q 2)))
  (is (= 3 (.getLength q)))))))

(deftest accumulating-mean
  (let [m (acc (AccumulatingMean.))]
    (is (= 1 (m 1)))
    (is (= 4 (m 7)))
    (is (= 4.666666666666667 (m 6)))
    (is (= 4.5 (m 4)))
    (is (= 4.6 (m 5)))))

(deftest rolling-mean
  (let [m (roll (RollingMean. 4) 4)]
    (is (Double/isNaN (m 1)))
    (is (Double/isNaN (m 7)))
    (is (Double/isNaN (m 6)))
    (is (= 4.5 (m 4)))
    (is (= 5.5 (m 5)))))