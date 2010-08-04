package infer;

    /// <summary> 
    /// The IndexableFixedLengthCompoundQueueOfDoubles is an optimized, indexable, fixed-length queue designed to solve 
    /// two problems for event driven calculations on rolling windows.  First, we discovered 
    /// performancce issues with a normal queue.  A normal queue is dynamicly resizing ad new 
    /// enemends are enqued and dequed.  A queue that serves as the data event caching mecahnism 
    /// to back rolling calculations is used in a specific pattern - data events cause both an 
    /// enqueue and a dequeue at the same time.  This pattern of useage can cause performace 
    /// issues for normal queues if it leads to dynamci sizing and resizing.  Since we know the 
    /// queue is fized length and has a certian useage pattern, we can impliment a different api;
    /// we can remove the Deque method and foled its behavior into the Enque method.  Moreover, 
    /// this can be done very efficiently on a fixed length array with a very fast indexing scheme 
    /// that continues wrapping indexes around the array and overwriting elements that are dequed.
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