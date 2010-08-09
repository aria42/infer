(ns infer.streaming
  (:import infer.RollingQueue
	   infer.Statistic
	   infer.RollingStatistic))

(defn roll
"Closes over the state of a streaming statistical calculation via a RollingStatistic.

Closes over the state of the window of data from the stream necessary for the calculation via a RollingQueue of length n.

WARNING: gnarley unguarded mutation in the RollingStatistic and RollingQueue.  This is not meant to be called raw by multiple concurrent callers.

It's native use case is in isolation.  Each rolling statistic instance being attached to a different data stream.

It can be wrapped with a checkpointer, but not a memoizer.

It can be wrapped with a lock to be safe for parallel updates to a single rolling statistic.
"
 [#^RollingStatistic stat n]
  (let [q (RollingQueue. n)]
    (fn [x]
      (if (.isPrimed q) 
      (.calculate stat x (.enqueue q x))
      (let [pr (.prime stat x)
	    _ (.enqueue q x)]
	(if (.isPrimed q) pr Double/NaN))))))

(defn acc
"Closes over the state of a streaming statistical calculation via a Statistic.

WARNING: gnarley unguarded mutation in the Statistic.  This is not meant to be called raw by multiple concurrent callers.

It's native use case is in isolation.  Each rolling statistic instance being attached to a different data stream.

It can be wrapped with a checkpointer, but not a memoizer.

It can be wrapped with a lock to be safe for parallel updates to a single rolling statistic.
"
[#^Statistic stat]
(fn [x] (.calculate stat x)))


;;http://en.wikipedia.org/wiki/Selection_algorithm