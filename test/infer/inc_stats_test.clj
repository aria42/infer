(ns infer.inc-stats-test
  (:use clojure.test)
  (:import infer.RollingQueue))

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