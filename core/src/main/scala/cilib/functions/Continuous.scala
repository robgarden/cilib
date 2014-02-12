package cilib.functions.continuous

import cilib.DualSolution
import cilib.math.Dual
import cilib.math.Dual.implicits._
import cilib.math.Dual.operations._

import spire.implicits._
import spire.math._

trait ContinuousFunction {
  def apply(x: DualSolution): Dual
}

object AbsoluteValue extends ContinuousFunction {
  def apply(x: DualSolution) = x.map(absD(_)).reduce(_ + _)
}

object Rosenbrock extends ContinuousFunction {
  def apply(x: DualSolution) = x.sliding(2).map {
      case Vector(xi, xi1) => ((xi1 - xi ** 2) ** 2) * 100 + (xi - 1) ** 2
  }.reduce(_ + _)
}

object Rastrigin extends ContinuousFunction {
  def apply(x: DualSolution) = x.map { xi =>
      xi ** 2 - cosD(xi * 2 * pi) * 10
    }.reduce(_ + _) * (10 + x.size)
}

object Salomon extends ContinuousFunction {
  def apply(x: DualSolution) = {
    val sqrtSquares = sqrtD(Sphere(x))
    -cosD(sqrtSquares * 2 * pi) + (sqrtSquares * 0.1) + 1
  }
}

object Sphere extends ContinuousFunction {
  def apply(x: DualSolution) = x.map(_ ** 2).reduce(_ + _)
}

// object Step extends ContinuousFunction {
//  def apply(x: DualSolution) = x.map(xi => (floor(xi) + 0.5) ** 2).sum
// }

object Vincent extends ContinuousFunction {
  def apply(x: DualSolution) = x.map(xi => sinD(logD(xi) * 10.0)).reduce(_ + _)
}

object EqualMaxima extends ContinuousFunction {
  def apply(x: DualSolution) = x.map(xi => sinD(xi * 5 * pi) ** 6).reduce(_ + _)
}
