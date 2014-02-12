import scalaz._

import language.implicitConversions

import spire.algebra.Field
import spire.algebra.VectorSpace
import spire.implicits._

import cilib.math.Dual

package object cilib {

  // Aliases
  type Solution = Vector[Double]
  def Solution(xs: Double*) = Vector(xs: _*)
  type DualSolution = Vector[Dual]
  implicit def solution2DualSolution(x: Solution): DualSolution = Dual.solution(x)

  sealed trait Positive
  sealed trait Negative

  def positive(d: Double): Option[Double @@ Positive] =
    if (d > 0.0) Tag.subst(Some(d))
    else None

  def negative(d: Double): Option[Double @@ Negative] =
    if (d < 0.0) Tag.subst(Some(d))
    else None

}
