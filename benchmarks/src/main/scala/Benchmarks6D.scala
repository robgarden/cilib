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

object Benchmarks6D {

  type Sized6[A] = (A, A, A, A, A, A)

  def toSized6[F[_]: Foldable, A](x: F[A]): Option[Sized6[A]] =
    (x.index(0) |@| x.index(1) |@| x.index(2) |@| x.index(3) |@| x.index(4) |@| x.index(5)) {
      (_, _, _, _, _, _)
    }

  def biggsEXP6[A: Field : Trig : Monoid](x: Sized6[A]) = {
    val (x1, x2, x3, x4, x5, x6) = x
    (1 to 13).toList.foldMap { i =>
      val ti = 0.1 * i
      val yi = exp(-ti) - 5.0 * exp(-10 * ti) + 3.0 * exp(-4.0 * ti)
      (x3 * exp(-ti * x1) - x4 * exp(-ti * x2) + x6 * exp(-ti * x5) - yi) ** 2
    }
  }

  def hartman6[A: Field : Trig](x: Sized6[A]) = {
    val y = List(x._1, x._2, x._3, x._4, x._5, x._6)

    val a = List(
      List(10.0, 3.00, 17.0, 3.50, 1.70, 8.00),
      List(0.05, 10.0, 17.0, 0.10, 8.00, 14.0),
      List(3.00, 3.50, 1.70, 10.0, 17.0, 8.00),
      List(17.0, 8.00, 0.05, 10.0, 0.10, 14.0)
    )

    val c = List(1.0, 1.2, 3.0, 3.2)

    val p = List(
      List(0.1312, 0.1696, 0.5569, 0.0124, 0.8283, 0.5886),
      List(0.2329, 0.4135, 0.8307, 0.3736, 0.1004, 0.9991),
      List(0.2348, 0.1451, 0.3522, 0.2883, 0.3047, 0.6650),
      List(0.4047, 0.8828, 0.8732, 0.5743, 0.1091, 0.0381)
    )

    val ts = for { i <- 0 until 4
      power = for (j <- 0 until 6) yield a(i)(j) * ((y(j) - p(i)(j)) ** 2)
    } yield c(i) * exp(-power.qsum)

    -ts.qsum
  }

}
