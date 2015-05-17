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

object Benchmarks4D {

  type Sized4[A] = (A, A, A, A)

  def toSized4[F[_]: Foldable, A](x: F[A]): Option[Sized4[A]] =
    (x.index(0) |@| x.index(1) |@| x.index(2) |@| x.index(3)) { (_, _, _, _) }

  def biggsEXP4[A: Field : Trig : Monoid](x: Sized4[A]) = {
    val (x1, x2, x3, x4) = x
    (1 to 10).toList.foldMap { i =>
      val ti = 0.1 * i
      val yi = exp(-ti) - 5.0 * exp(-10 * ti)
      (x3 * exp(-ti * x1) - x4 * exp(-ti * x2) - yi) ** 2
    }
  }

  def colville[A: Field](x: Sized4[A]) = {
    val (x1, x2, x3, x4) = x
    val t1 = 100.0 * ((x1 - (x2 ** 2)) ** 2) + ((1.0 - x1) ** 2)
    val t2 = 90.0 * ((x4 - x3) ** 2) + ((1.0 - x3) ** 2)
    val t3 = 10.1 * (((x2 - 1.0) ** 2) + ((x4 - 1.0) ** 2))
    val t4 = 19.8 * (x2 - 1.0) * (x4 - 1.0)

    t1 + t2 + t3 + t4
  }

  def deVilliersGlasser1[A: Field : NRoot : Trig : Monoid](x: Sized4[A]) = {
    val (x1, x2, x3, x4) = x
    (1 to 24).toList.foldMap { i =>
      val ti = 0.1 * (i - 1)
      val yi = 60.137 * (1.371 ** ti) * sin(3.112 * ti + 1.761)
      val t1 = x1 * (x2 ** ti)
      val t2 = sin(x3 * ti + x4)
      val t3 = yi
      (t1 * t2 - t3) ** 2
    }
  }

  def gear[A: Field : IsReal](x: Sized4[A]) = {
    val (x1, x2, x3, x4) = (
      floor(x._1), floor(x._2), floor(x._3), floor(x._4)
    )
    val t1 = 1.0 / 6.931
    val numer = x1 * x2
    val denom = x3 * x4
    (t1 - (numer / denom)) ** 2
  }

  def kowalik[A: Field : Monoid](x: Sized4[A]) = {
    val (x1, x2, x3, x4) = x
    val b = List(
      4.0, 2.0, 1.0, 0.5, 0.25, 1.0 / 6.0, 0.125,
      0.1, 1.0 / 12.0, 1.0 / 14.0, 0.0625
    )

    val a = List(
      0.1957, 0.1947, 0.1735, 0.1600, 0.0844, 0.0627,
      0.0456, 0.0342, 0.0323, 0.0235, 0.0246
    )

    val ts = a.zip(b).map { case (ai, bi) =>
      val numer = x1 * ((bi ** 2) + bi * x2)
      val denom = (bi ** 2) + bi * x3 + x4
      ai - (numer / denom)
    }
    ts.foldMap(_ ** 2)
  }

  def mieleCantrell[A: Field : Trig](x: Sized4[A]) = {
    val (x1, x2, x3, x4) = x
    val t1 = (exp(-x1) - x2) ** 4
    val t2 = 100 * ((x2 - x3) ** 6)
    val t3 = tan(x3 - x4) ** 4
    val t4 = x1 ** 8
    t1 + t2 + t3 + t4
  }

  def powell[A: Field](x: Sized4[A]) = {
    val (x1, x2, x3, x4) = x
    val t1 = (x3 + 10 * x1) ** 2
    val t2 = 5.0 * ((x2 - x4) ** 2)
    val t3 = (x1 - 2 * x2) ** 4
    val t4 = 10.0 * ((x3 - x4) ** 4)
    t1 + t2 + t3 + t4
  }

  def powerSum[A: Ring : Monoid](x: Sized4[A]) = {
    val b = List(8, 18, 44, 114)
    val (x1, x2, x3, x4) = x
    val xs = List(x1, x2, x3, x4)

    b.zipWithIndex.foldMap {
      case (bk, i) =>
        val k = i + 1
        val t = xs.foldMap(xi => xi ** k)
        (t - bk) ** 2
    }
  }

  def wood[A: Field](x: Sized4[A]) = {
    val (x1, x2, x3, x4) = x
    val t1 = 100 * (((x1 ** 2) - x2) ** 2)
    val t2 = (x1 - 1) ** 2 + (x3 - 1) ** 2
    val t3 = 90 * ((x3 ** 2 - x4) ** 2)
    val t4 = 10.1 * ((x2 - 1) ** 2)
    val t5 = (x4 - 1) ** 2 + 19.8 * (x2 - 1) * (x4 - 1)
    t1 + t2 + t3 + t4 + t4
  }

}
