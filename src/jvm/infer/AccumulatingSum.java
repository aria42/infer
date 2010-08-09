package infer;

public class AccumulatingSum implements Statistic
{
  private double value;

  public AccumulatingSum()
  {
    value = 0;
  }

  public double calculate(double x)
  {
    value += x;
    return value;
  }
}