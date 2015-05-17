package cilib
package benchmarks

import _root_.scala.Predef.{any2stringadd => _, _}

import scalaz.{Functor,Foldable,Foldable1,Applicative,Monoid,NonEmptyList,Id,OneAnd}
import scalaz.syntax.apply._
import scalaz.syntax.foldable1._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.std.option._

import spire.math._
import spire.algebra.{Monoid => _, _}
import spire.implicits._

object Benchmarks1D {

  type Sized1[A] = Id.Id[A]

  def toSized1[F[_]: Foldable, A](x: F[A]): Option[Sized1[A]] = x.index(0)

  def centralTwoPeakTrap[A: Field : Order](x1: Sized1[A]) =
    if      (x1 < 0)   0.0 * x1
    else if (x1 <= 10) (-160.0 / 10.0) * x1
    else if (x1 <= 15) (-160.0 / 5.0) * (15.0 - x1)
    else if (x1 <= 20) (-200.0 / 5.0) * (x1 - 15.0)
    else               (0.0 * x1) - 200.0

}
