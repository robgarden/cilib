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

object Benchmarks3D {

  type Sized3[A] = (A, A, A)

  def toSized3[F[_]: Foldable, A](x: F[A]): Option[Sized3[A]] =
    (x.index(0) |@| x.index(1) |@| x.index(2)) { (_, _, _) }

  def biggsEXP3[A: Field : Trig : Monoid](x: Sized3[A]) = {
    val (x1, x2, x3) = x
    (1 to 10).toList.foldMap { i =>
      val ti = 0.1 * i
      val yi = exp(-ti) - 5.0 * exp(-10 * ti)
      (exp(-ti * x1) - x3 * exp(-ti * x2) - yi) ** 2
    }
  }

  def boxBettsQuadraticSum[A: Field : Trig : Monoid](k: Int)(x: Sized3[A]) = {
    val (x1, x2, x3) = x
    (1 to k).toList.foldMap { i =>
      val co = -0.1 * i
      val t1 = exp(co * x1)
      val t2 = exp(co * x2)
      val t3 = (exp(co) - exp(-i.toDouble)) * x3
      (t1 - t2 - t3) ** 2
    }
  }

  def brad[A: Field : Monoid](x: Sized3[A]) = {
    val (x1, x2, x3) = x
    val y = List(
      0.14, 0.18, 0.22, 0.25, 0.29,
      0.32, 0.35, 0.39, 0.37, 0.58,
      0.73, 0.96, 1.34, 2.10, 4.39
    )

    (1 to 15).toList.foldMap { ui =>
      val vi = 16 - ui
      val wi = spire.math.min(ui, vi)
      ((y(ui - 1) - x1 - ui) / (vi * x2 + wi * x3)) ** 2
    }
  }

  def hartman3[A: Field : Trig](x: Sized3[A]) = {
    val y = List(x._1, x._2, x._3)

    val a = List(
      List(3.0, 10.0, 30.0),
      List(0.1, 10.0, 35.0),
      List(3.0, 10.0, 30.0),
      List(0.1, 10.0, 35.0)
    )

    val c = List(1.0, 1.2, 3.0, 3.2)

    val p = List(
      List(0.6890, 0.1170, 0.2673),
      List(0.4699, 0.4387, 0.7470),
      List(0.1091, 0.8732, 0.5547),
      List(0.0381, 0.5743, 0.8828)
    )

    val ts = for {
      i <- 0 until 4
      power = for (j <- 0 until 3) yield a(i)(j) * ((y(j) - p(i)(j)) ** 2)
    } yield c(i) * exp(-power.qsum)

    -ts.qsum
  }

  def helicalValley[A: Field : NRoot : Order : Trig](x: Sized3[A]) = {
    val (x1, x2, x3) = x

    val theta = {
      val t  = if (x1 >= (x1 * 0)) 0.0 else 0.5
      atan((x2 / x1) + t) / (2 * pi)
    }

    val t1 = (x2 - 10.0 * theta) ** 2
    val t2 = sqrt((x1 ** 2) + (x2 ** 2)) - 1.0
    val t3 = x3 ** 2

    100.0 * (t1 + t2) + t3
  }

  def wolfe[A: Field : NRoot](x: Sized3[A]) = {
    val (x1, x2, x3) = x
    (4.0 / 3.0) * (((x1 ** 2) + (x2 ** 2)) ** 0.75) + x3
  }

}
