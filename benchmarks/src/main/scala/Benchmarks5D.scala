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

object Benchmarks5D {

  type Sized5[A] = (A, A, A, A, A)

  def toSized5[F[_]: Foldable, A](x: F[A]): Option[Sized5[A]] =
    (x.index(0) |@| x.index(1) |@| x.index(2) |@| x.index(3) |@| x.index(4)) { (_, _, _, _, _) }

  def biggsEXP5[A: Field : Trig : Monoid](x: Sized5[A]) = {
    val (x1, x2, x3, x4, x5) = x
    (1 to 11).toList.foldMap { i =>
      val ti = 0.1 * i
      val yi = exp(-ti) - 5.0 * exp(-10 * ti) + 3.0 * exp(-4.0 * ti)
      (x3 * exp(-ti * x1) - x4 * exp(-ti * x2) + 3.0 * exp(-ti * x5) - yi) ** 2
    }
  }

  def deVilliersGlasser2[A: Field : NRoot : Trig : Monoid](x: Sized5[A]) = {
    val (x1, x2, x3, x4, x5) = x
    (1 to 16).toList.foldMap { i =>
      val ti = 0.1 * (i - 1)
      val yi = 53.81 * (1.27 ** ti) * tanh(3.012 * ti + sin(2.13 * ti)) *
        cos(exp(0.507) * ti)
      val t1 = x1 * (x2 ** ti)
      val t2 = tanh(x3 * ti + sin(x4 * ti))
      val t3 = cos(ti * exp(x5))
      val t4 = yi

      (t1 * t2 * t3 - t4) ** 2
    }
  }

  def dolan[A: Field : Signed : Trig](x: Sized5[A]) = {
    val (x1, x2, x3, x4, x5) = x
    val t1 = (x1 + 1.7 * x2) * sin(x1)
    val t2 = -1.5 * x3 - 0.1 * x4 * cos(x4 + x5 - x1)
    val t3 = 0.2 * (x5 ** 2) - x2 -1.0
    abs(t1 + t2 + t3)
  }

}
