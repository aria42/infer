package infer;

public interface RollingStatistic
{
  double prime(double x);
  double calculate(double xnew, double xold);
}