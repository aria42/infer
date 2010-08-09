package infer;

public class RollingSum implements RollingStatistic
{
  private final AccumulatingSum initialSum;
  private double sum;

  public RollingSum()
  {
    initialSum = new AccumulatingSum();
  }

  public double calculate(double xnew, double xold)
  {
    sum = sum - xold + xnew;
    return sum;
  }

  public double prime(double x)
  {
    sum = initialSum.calculate(x);
    return sum;
  }
}