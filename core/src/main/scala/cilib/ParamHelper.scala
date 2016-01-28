package cilib

object ParamHelper {
  def increasing(lo: Double, hi: Double, n: Int) =
    for (i <- 0 to n) yield lo + (i * ((hi - lo) / n))

  def decreasing(lo: Double, hi: Double, n: Int) =
    increasing(lo, hi, n).reverse
}

