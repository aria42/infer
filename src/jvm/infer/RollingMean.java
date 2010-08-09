package infer;

public class RollingMean implements RollingStatistic
{
  private final int length;
  private int count;
  private final RollingSum sum;

  public RollingMean(int length)
  {
    sum = new RollingSum();
    count = 0;
    this.length = length;
  }

  public double calculate(double xnew, double xold)
  {
    return sum.calculate(xnew, xold)/length;
  }

  public double prime(double x)
  {
    return sum.prime(x) / ++count;
  }
}