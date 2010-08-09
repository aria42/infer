package infer;

public class AccumulatingMean implements Statistic
{
  private AccumulatingSum sum;
  private double length;

  public AccumulatingMean()
  {
    sum = new AccumulatingSum();
    length = 0;
  }

  public double calculate(double x)
  {
    return  sum.calculate(x) / ++length; 
  }
}