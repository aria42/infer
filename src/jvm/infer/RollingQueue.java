package infer;

    /// <summary> 
    /// The Rolling is an optimized, indexable, fixed-length queue for event driven calculations on rolling windows.
    /// Mormal queues dynamicly resize, but for the rolling update case, data events always cause both an 
    /// enqueue and a dequeue at the same time.
    /// We can remove the Deque method and fold its behavior into the Enque method.
    /// This can be done very efficiently on a fixed length array with an indexing scheme wraps indexes around
    /// the array and overwrites elements that are dequed.
    /// </summary>
    public class RollingQueue
    {
        private final double[] backingArray;
        private int tail;
        private final int length;
        public boolean isPrimed;

        public RollingQueue(int i)
        {
            length = i;
            backingArray = new double[i];
            for (int j = 0; j < i; j++)
            {
                backingArray[j] = Double.NaN;
            }
        }

      public int getLength()
        {
            return isPrimed ? length : tail;
        }

        public double enqueue(double element)
        {
            double tailValue = backingArray[tail];
            backingArray[tail] = element;
            tail = tail + 1;
            if (tail > length - 1) tail = 0;
            if (!isPrimed) isPrimed = tail == 0;
            return tailValue;
        }

        public double getAtLookback(int n)
        {
            int index = wrapFromTheBack(tail - n - 1);
            return backingArray[index];
        }

        public double getAtIndex(int i)
        {
            return isPrimed ? getAtLookback(length - 1 - i) : backingArray[0];
        }

        private int wrapFromTheBack(int index)
        {
            return (index < 0) ? length + index : index;
        }

        public double[] cloneElements()
        {
            if (isPrimed) return (double[]) backingArray.clone();
            double[] elements = new double[tail];
	    System.arraycopy(backingArray, 0, elements, 0, tail);
            return (double[]) elements.clone();
        }
    }