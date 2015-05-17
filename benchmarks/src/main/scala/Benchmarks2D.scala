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

object Benchmarks2D {

  type Sized2[A] = (A, A)

  def toSized2[F[_]: Foldable, A](x: F[A]): Option[Sized2[A]] =
    (x.index(0) |@| x.index(1)) { (_, _) }

  def adjiman[A: Field : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    cos(x1) * sin(x2) - (x1) / (x2 ** 2 + 1)
  }

  def bartelsConn[A: Ring : Signed : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    abs(x1 ** 2 + x2 ** 2 + x1 * x2) + abs(sin(x1)) + abs(cos(x2))
  }

  def beale[A: Field : IsReal](x: Sized2[A]) = {
    val (x1, x2) = x
    (1.5 - x1 + x1 * x2) ** 2 +
    (2.25 - x1 + x1 * (x2 ** 2)) ** 2 +
    (2.625 - x1 + x1 * (x2 ** 3)) ** 2
  }

  def biggsEXP2[A: Field : Trig : Monoid](x: Sized2[A]) = {
    val (x1, x2) = x
    (1 to 10).toList.foldMap { i =>
      val ti = 0.1 * i
      val yi = exp(-ti) - 5.0 * exp(-10 * ti)
      (exp(-ti * x1) - 5.0 * exp(-ti * x2) - yi) ** 2
    }
  }

  def bird[A: Ring : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    sin(x1) * exp((1 - cos(x2)) ** 2) +
    cos(x2) * exp((1 - sin(x1)) ** 2) + (x1 - x2) ** 2
  }

  def bohachevsky1[A: Field : IsReal : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    (x1 ** 2) + 2 * (x2 ** 2) - 0.3 *
    cos(3 * pi * x1) - 0.4 * cos(4 * pi * x2) + 0.7
  }

  def bohachevsky2[A: Field : IsReal : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    (x1 ** 2) + 2 * (x2 ** 2) - 0.3 *
    cos(3 * pi * x1) * cos(4 * pi * x2) + 0.3
  }

  def bohachevsky3[A: Field : IsReal : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    (x1 ** 2) + 2 * (x2 ** 2) - 0.3 *
    cos(3 * pi * x1 + 4 * pi * x2) + 0.3
  }

  def booth[A: IsReal : Ring](x: Sized2[A]) = {
    val (x1, x2) = x
    (x1 + 2 * x2 - 7) ** 2 + (2 * x1 + x2 - 5) ** 2
  }

  def braninRCOS1[A: Field : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = (x2 - (5.1 / (4.0 * (pi ** 2))) * (x1 ** 2) +
      (5.0 / pi) * x1 - 6.0) ** 2
    val t2 = 10.0 * (1.0 - 1 / (8.0 * pi)) * cos(x1)
    t1 + t2 + 10.0
  }

  def braninRCOS2[A: Field : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = (-1.275 * (x1 ** 2)/(pi ** 2) + (5.0 * x1) / pi + x2 - 6) ** 2
    val t2 = (10.0 - 5.0 / (4.0 * pi)) * cos(x1) * cos(x2)
    val t3 = log((x1 ** 2) + (x2 ** 2) + 1.0) + 10.0
    t1 + t2 + t3
  }

  def bukin2[A: Field](x: Sized2[A]) = {
    val (x1, x2) = x
    100 * (x2 - 0.01 * (x1 ** 2) + 1) ** 2 + 0.01 * ((x1 + 10) ** 2)
  }

  def bukin2Adapted[A: Field](x: Sized2[A]) =
    bukin2(x) ** 2

  def bukin4[A: Field : Signed](x: Sized2[A]) = {
    val (x1, x2) = x
    100 * (x2 ** 2) + 0.01 * abs(x1 + 10)
  }

  def bukin6[A: Field : NRoot : Signed](x: Sized2[A]) = {
    val (x1, x2) = x
    100.0 * sqrt(abs(x2 - 0.01 * (x1 ** 2))) + 0.01 * abs(x1 + 10.0)
  }

  def chichinadze[A: Field : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = (x1 ** 2) - (12.0 * x1) + 11.0
    val t2 = 10.0 * cos(pi * (x1 / 2.0)) + 8.0 * sin(5.0 * pi * x1)
    val t3 = ((1.0 / 5.0) ** 0.5) * exp(-0.5 * ((x2 - 0.5) ** 2))

    t1 + t2 - t3
  }

  def cube[A: Field](x: Sized2[A]) = {
    val (x1, x2) = x
    100 * ((x2 - (x1 ** 3)) ** 2) + ((1.0 - x1) ** 2)
  }

  def damavandi[A: Field : IsReal : Signed : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    if ((x1 != 2.0) && (x2 != 2.0)) {
      val numer = sin(pi * (x1 - 2)) * sin(pi * (x2 - 2))
      val denom = (pi ** 2) * (x1 - 2) * (x2 - 2)
      val factor1 = 1 - (abs(numer / denom) ** 5)
      val factor2 = 2 + ((x1 - 7) ** 2) + 2 * ((x2 - 7) ** 2)
      (factor1 * factor2).some
    } else none
  }

  def decanomial[A: Field : Signed : Monoid](x: Sized2[A]) = {
    val (x1, x2) = x
    val coX1 = List(1, -20, 180, -960, 3360, -8064,
                    13340, -15360, 11520, -5120, 2624)
    val coX2 = List(1, 12, 54, 108, 81)

    def one(l: List[Int], xi: A) =
      abs(l.zipWithIndex.foldMap {
        case (ci, i) => ci * (xi ** (l.length - 1 - i))
      })

    0.001 * ((one(coX2, x2) + one(coX1, x1)) ** 2)
  }


  def deckkersAarts[A: Field](x: Sized2[A]) = {
    val (x1, x2) = (x._1 ** 2, x._2 ** 2)
    val t1 = (10 ** 5) * x1 + x2
    val t2 = (x1 + x2) ** 2
    val t3 = (1.0 / (10 ** 5)) * ((x1 + x2) ** 4)

    t1 - t2 + t3
  }

  def easom[A: Field : IsReal : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    -cos(x1) * cos(x2) * exp(-((x1 - pi) ** 2 + (x2 - pi) ** 2))
  }

  def elAttarVidyasagarDutta[A: Ring](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = ((x1 ** 2) + x2 - 10) ** 2
    val t2 = (x1 + (x2 ** 2) - 7) ** 2
    val t3 = ((x1 ** 2) + (x2 ** 3) - 1) ** 2
    t1 + t2 + t3
  }

  def exponential2[A: Field : Trig : Monoid](x: Sized2[A]) = {
    val (x1, x2) = x
    (0 to 9).toList.foldMap { i =>
      val t1 = 1.0 * exp(-i * x1 / 10.0)
      val t2 = 5.0 * exp(-i * x2 / 10.0)
      val t3 = 1.0 * exp(-i / 10.0)
      val t4 = 5.0 * exp(-i.toDouble)
      (t1 - t2 - t3 + t4) ** 2
    }
  }

  def freudensteinRoth[A: Ring](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = (x1 - 13 + ((5 - x2) * x2 - 2) * x2) ** 2
    val t2 = (x1 - 29 + ((x2 + 1) * x2 -14) * x2) ** 2
    t1 + t2
  }

  def giunta[A: Field : Trig : Monoid](x: Sized2[A]) =
    0.6 + List(x._1, x._2).foldMap { xi =>
      val factor = (16.0 / 15.0) * xi - 1
      val t1 = sin(factor)
      val t2 = t1 ** 2
      val t3 = (1.0 / 50.0) * sin(4 * factor)
      t1 + t2 + t3
    }

  def goldsteinPrice1[A: Ring](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = 1 + ((x1 + x2 + 1) ** 2) * (19 - 14 * x1 + 3 * (x1 ** 2) -
      14 * x2 + 6 * x1 * x2 + 3 * (x2 ** 2))
    val t2 = 30 + ((2 * x1 - 3 * x2) ** 2) * (18 - 32 * x1 + 12 *
      (x1 ** 2) + 48 * x2 - 36 * x1 * x2 + 27 * (x2 ** 2))
    t1 * t2
  }

  def goldsteinPrice2[A: Field : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = exp(0.5 * (((x1 ** 2) + (x2 ** 2) - 25) ** 2))
    val t2 = sin(4 * x1 - 3 * x2) ** 4
    val t3 = 0.5 * ((2 * x1 + x2 - 10) ** 2)
    t1 + t2 + t3
  }

  def hansen[A: Ring : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = (0 to 4).map { i =>
      (i + 1) * cos(i * x1 + i + 1)
    }.qsum
    val t2 = (0 to 4).map { j =>
      (j + 1) * cos((j + 2) * x2 + j + 1)
    }.qsum

    t1 * t2
  }

  def himmelblau[A: Ring](x: Sized2[A]) = {
    val (x1, x2) = x
    (x1 ** 2 + x2 - 11) ** 2 + (x1 + x2 ** 2 - 7) ** 2
  }

  def hosaki[A: Field : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = 1 - 8 * x1 + 7 * (x1 ** 2)
    val t2 = (7.0 / 3.0) * (x1 ** 3)
    val t3 = (1.0 / 4.0) * (x1 ** 4)
    val t4 = (x2 ** 2) * exp(-x2)
    (t1 - t2 + t3) * t4
  }

  def jennrichSampson[A: Ring : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    val ts = for { i <- 1 to 10
      t1 = 2 + 2 * i
      t2 = exp(i * x1) + exp(i * x2)
    } yield (t1 - t2) ** 2

    ts.qsum
  }

  def judge[A: Field : Monoid](x: Sized2[A]) = {
    val (x1, x2) = x
    val A = List(
      4.284, 4.149, 3.877, 0.533, 2.211,
      2.389, 2.145, 3.231, 1.998, 1.379,
      2.106, 1.428, 1.011, 2.179, 2.858,
      1.388, 1.651, 1.593, 1.046, 2.152
    )

    val B = List(
      0.286, 0.973, 0.384, 0.276, 0.973,
      0.543, 0.957, 0.948, 0.543, 0.797,
      0.936, 0.889, 0.006, 0.828, 0.399,
      0.617, 0.939, 0.784, 0.072, 0.889
    )

    val C = List(
      0.645, 0.585, 0.310, 0.058, 0.455,
      0.779, 0.259, 0.202, 0.028, 0.099,
      0.142, 0.296, 0.175, 0.180, 0.842,
      0.039, 0.103, 0.620, 0.158, 0.704
    )

    val mappedB = B.map(_ * x2)
    val mappedC = C.map(_ * (x2 ** 2))

    val t1 = mappedB.zip(mappedC).map { case (ai, bi) => ai + bi }
    val t2 = t1.map(_ + x1)

    t2.zip(A).foldMap {
      case (t2, ai) => (t2 - ai) ** 2
    }
  }

  def keane[A: Field : NRoot : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    val numer = (sin(x1 - x2) ** 2) * (sin(x1 + x2) ** 2)
    val denom = sqrt((x1 ** 2) + (x2 ** 2))
    numer / denom
  }

  def leon[A: Ring](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = 100 * ((x2 - (x1 ** 2)) ** 2)
    val t2 = (1 - x1) ** 2

    t1 + t2
  }

  def levy5[A: Field : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = (for (i <- 1 to 5) yield i * cos((i - 1) * x1 + i)).qsum
    val t2 = (for (j <- 1 to 5) yield j * cos((j + 1) * x2 + j)).qsum
    val t3 = (x1 + 1.42513) ** 2
    val t4 = (x2 + 0.80032) ** 2
    t1 * t2 + t3 + t4
  }

  def levy13[A: Field : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = ((x1 - 1) ** 2) * ((sin(3 * pi * x2) ** 2) + 1)
    val t2 = ((x2 - 1) ** 2) * ((sin(2 * pi * x2) ** 2) + 1)
    val t3 = sin(3 * pi * x1) ** 2
    t1 + t2 + t3
  }

  def matyas[A: Field : IsReal](x: Sized2[A]) = {
    val (x1, x2) = x
    0.26 * (x1 ** 2 + x2 ** 2) - 0.48 * x1 * x2
  }

  def mcCormick[A: Field : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = sin(x1 + x2) + ((x1 - x2) ** 2)
    val t2 = -1.5 * x1 + 2.5 * x2 + 1
    t1 + t2
  }

  def mishra5[A: Field : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = sin((cos(x1) + cos(x2)) ** 2) ** 2
    val t2 = cos((sin(x1) + sin(x2)) ** 2) ** 2
    val t3 = 0.01 * (x1 + x2)
    ((t1 + t2 + x1) ** 2) + t3
  }

  def mishra8[A: Field : Signed](x: Sized2[A]) = {
    val (x1, x2) = x
    val coX1 = List(1, -20, 180, -960, 3360, -8064,
      13340, -15360, 11520, -5120, 2624)
    val coX2 = List(1, 12, 54, 108, 81)

    val t1 = abs(coX1.zipWithIndex.map {
      case (ci, i) => ci * (x1 ** (coX1.length - 1 - i))
    }.qsum)

    val t2 = abs(coX2.zipWithIndex.map {
      case (ci, i) => ci * (x2 ** (coX2.length - 1 - i))
    }.qsum)

    0.001 * ((t1 * t2) ** 2)
  }

  def parsopoulus[A: Field : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    (cos(x1) ** 2) + (sin(x2) ** 2)
  }

  def penHolder[A: Field : NRoot : Signed : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = abs(1 - (sqrt((x1 ** 2) + (x2 ** 2)) / pi))
    val t2 = cos(x1) * cos(x2)
    val expon = abs(exp(t1) * t2) ** -1
    -exp(-expon)
  }

  def quadratic[A: Field](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = -3803.84 - 138.08 * x1
    val t2 = -232.92 * x2 + 128.08 * (x1 ** 2)
    val t3 = 203.64 * (x2 ** 2) + 182.25 * x1 * x2
    t1 + t2 + t3
  }

  def schwefel236[A: Field](x: Sized2[A]) = {
    val (x1, x2) = x
    -x1 * x2 * (72.0 - 2.0 * x1 - 2.0 * x2)
  }

  def schwefel26[A: Order : Ring : Signed](x: Sized2[A]) = {
    val (x1, x2) = x
    spire.math.max(abs(x1 + 2 * x2 - 7), abs(2 * x1 + x2 - 5))
  }

  def shubert[F[_]: Foldable, A: Field : Trig : Monoid](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = (1 to 5).toList.foldMap(j => j * cos((j + 1) * x1 + j))
    val t2 = (1 to 5).toList.foldMap(j => j * cos((j + 1) * x2 + j))
    t1 * t2
  }

  def sixHumpCamelback[A: Field](x: Sized2[A]) = {
    val (x1, x2) = x
    val tX1 = 4 * (x1 ** 2) - 2.1 * (x1 ** 4) + (1.0 / 3.0) * (x1 ** 6)
    val tX2 = x1 * x2 - 4 * (x2 ** 2) + 4 * (x2 ** 4)
    tX1 + tX2
  }

  def threeHumpCamelback[A: Field : IsReal](x: Sized2[A]) = {
    val (x1, x2) = x
    2 * (x1 ** 2) - 1.05 * (x1 ** 4) + ((x1 ** 6) / 6) + x1 * x2 + x2 ** 2
  }

  def trecanni[A: Ring](x: Sized2[A]) = {
    val (x1, x2) = x
    x1 ** 4 + 4 * (x1 ** 3) + 4 * (x1 ** 2) + (x2 ** 2)
  }

  def ursem1[A: Field : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    -sin(2.0 * x1 - 0.5 * pi) - 3.0 * cos(x2) - 0.5 * x1
  }

  def ursem3[A: Field : Trig : Signed](x: Sized2[A]) = {
    val (x1, x2) = x
    val co1 = -sin(2.2 * pi * x1 + 0.5 * pi)
    val co2 = -sin(2.2 * pi * x2 + 0.5 * pi)
    val t1 = co1 *  ((2.0 - abs(x1)) / (2.0)) * ((3.0 - abs(x1)) / (2.0))
    val t2 = co2 *  ((2.0 - abs(x2)) / (2.0)) * ((3.0 - abs(x2)) / (2.0))
    t1 + t2
  }

  def ursem4[A: Field : NRoot : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = -3.0 * sin(0.5 * pi * x1 + 0.5 * pi)
    val t2 = (2.0 - sqrt((x1 ** 2) + (x2 ** 2))) / 4.0
    t1 * t2
  }

  def ursemWaves[A: Field : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = -0.9 * (x1 ** 2)
    val t2 = ((x2 ** 2) - 4.5 * (x2 ** 2)) * x1 * x2
    val t3 = 4.7 * cos(3.0 * x1 - (x2 ** 2) * (2.0 + x1))
    val t4 = sin(2.5 * pi * x1)
    t1 + t2 + t3 * t4
  }

  def venterSobiezcczanskiSobieski[A: Field : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = (x1 ** 2) - 100.0 * (cos(x1) ** 2)
    val t2 = -100.0 * cos((x1 ** 2) / 30.0) + (x2 ** 2)
    val t3 = -100.0 * (cos(x2) ** 2) - 100.0 * cos((x2 ** 2) / 30.0)
    t1 + t2 + t3
  }

  def wayburnSeader1[A: Field](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = ((x1 ** 6) + (x2 ** 4) - 17.0) ** 2
    val t2 = (2.0 * x1 + x2 - 4) ** 2
    t1 + t2
  }

  def wayburnSeader2[A: Field](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = (1.613 - 4.0 * ((x1 - 0.3125) ** 2) - 4.0 * ((x2 - 1.625) ** 2)) ** 2
    val t2 = (x2 - 1.0) ** 2
    t1 + t2
  }

  def wayburnSeader3[A: Field](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = 2.0 * ((x1 ** 3) / 3.0) - 8.0 * (x1 ** 2) + 33.0 * x1 - x1 * x2 + 5
    val t2 = (((x1 - 4.0) ** 2) + ((x2 - 5.0) ** 2) - 4.0) ** 2
    t1 + t2
  }

  def zettle[A: Field](x: Sized2[A]) = {
    val (x1, x2) = x
    (x1 ** 2 + x2 ** 2 - 2 * x1) ** 2 + x1 / 4.0
  }

  def zirilli1[A: Field](x: Sized2[A]) = {
    val (x1, x2) = x
    0.25 * (x1 ** 4) - 0.5 * (x1 ** 2) + 0.1 * x1 + 0.5 * (x2 ** 2)
  }

  def zirilli2[A: Field : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    0.5 * (x1 ** 2) + 0.5 * (1.0 - cos(2.0 * x1)) + (x2 ** 2)
  }

}
