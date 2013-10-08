package net.sourceforge.cilib.measurement.functionmetric

object Euclidean {
  def apply(p1: Point, p2: Point): Double = {
    math.sqrt(p1.x.zip(p2.x).map {
      case (a, b) =>
        (a - b) * (a - b)
    }.sum)
  }
}