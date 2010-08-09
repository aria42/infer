(ns infer.streaming)

;;WARNING: gnarlley unguarded mutation back here.  This is not meant to be called by multiple concurrent callers, it is meant to be used only in isolation.  It can be wrapped with a checkpointer, but not a memoizer.  It is stricly for use with updating streams.
(defn rolling [f p n]
  (let [q (RollingQueue. n)]
    (fn [x]
      (if (.isPrimed q) 
      (f x (.enqueue q x))
      (let [pr (p x (.enqueue q x))]
	(if (.isPrimed q) pr Double/NaN))))))

;; (defn sum [new old]
  
;;             sum = sum - compoundQueueOfDoublesfixedLengthQueue.Enqueue(d) + d;
;;             return sum;
;;         }

;;         protected override double prime(double d)
;;         {
;;             sum = initialSum.Calculate(d);
;;             compoundQueueOfDoublesfixedLengthQueue.Enqueue(d);
;;             return sum;
;;         }
;;     }


;;http://en.wikipedia.org/wiki/Selection_algorithm