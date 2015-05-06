package cilib

import _root_.scala.Predef.{any2stringadd => _, _}

import scalaz.{Functor,Foldable,Foldable1,Applicative,Monoid,NonEmptyList,Id,OneAnd}
import scalaz.syntax.apply._
import scalaz.syntax.foldable._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.std.option._

import spire.math._
import spire.algebra.{Monoid => _, _}
import spire.implicits._

object Functions {

  type Sized1[A] = Id.Id[A]
  type Sized2[A] = (A, A)
  type Sized3[A] = (A, A, A)
  type Sized4[A] = (A, A, A, A)
  type Sized5[A] = (A, A, A, A, A)

  type Sized1And[F[_], A] = OneAnd[F, A]
  case class Sized2And[F[_], A](a: A, b: A, rest: F[A])

  def toSized1[F[_]: Foldable, A](x: F[A]): Option[Sized1[A]] = x.index(0)

  def toSized2[F[_]: Foldable, A](x: F[A]): Option[Sized2[A]] =
    (x.index(0) |@| x.index(1)) { (_, _) }

  def toSized3[F[_]: Foldable, A](x: F[A]): Option[Sized3[A]] =
    (x.index(0) |@| x.index(1) |@| x.index(2)) { (_, _, _) }

  def toSized4[F[_]: Foldable, A](x: F[A]): Option[Sized4[A]] =
    (x.index(0) |@| x.index(1) |@| x.index(2) |@| x.index(3)) { (_, _, _, _) }

  def toSized5[F[_]: Foldable, A](x: F[A]): Option[Sized5[A]] =
    (x.index(0) |@| x.index(1) |@| x.index(2) |@| x.index(3) |@| x.index(4)) { (_, _, _, _, _) }

  // Functions
  def absoluteValue[F[_]: Foldable1, A: Signed: Monoid](x: F[A]) =
    x.foldMap(abs(_))

  def ackley[F[_]: Foldable1, A: Field : IsReal : NRoot : Trig : Monoid](x: F[A]) = {
    val n = x.length
    val sumcos = x.foldMap(x => cos(2 * pi * x))
    val sumsqr = x.foldMap(_ ** 2)

    -20 * exp(-0.2 * sqrt(sumsqr / n)) - exp(sumcos / n) + 20 + e
  }

  def adjiman[A: Field : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    cos(x1) * sin(x2) - (x1) / (x2 ** 2 + 1)
  }

  def alpine1[F[_]: Foldable1, A: Field : Signed : Trig : Monoid](x: F[A]) =
    x.foldMap(xi => abs((xi * sin(xi)) + (0.1 * xi)))

  def arithmeticMean[F[_]: Foldable1, A: NRoot : Monoid](x: F[A])(implicit A: Field[A]) = {
    val n = x.length
    val avg = x.fold / n
    val rootProd = x.foldLeft(A.one)(_ * _) ** (1.0 / n)

    (avg - rootProd) ** 2
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

  def boxBettsQuadraticSum[A: Field : Trig : Monoid](k: Int)(x: Sized3[A]) = {
    val (x1, x2, x3) = x
    (1 to k).toList.foldMap { i =>
      val i1 = i + 1.0
      val co = -0.1 * (i1)
      val t1 = exp(co * x1)
      val t2 = exp(co * x2)
      val t3 = exp((co - exp(-i1)) * x3)
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

  def braninRCOS1[A: Field : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = (x2 - (5.1 / (4.0 * (pi ** 2))) * (x1 ** 2) +
      (5.0 / pi) * x1 - 6.0) ** 2
    val t2 = 10.0 * (1.0 - 1 / (8.0 * pi)) * cos(x1)
    t1 + t2 + 10.0
  }

  def brent[F[_]: Foldable1, A: Ring : Trig : Monoid](x: F[A]) =
    x.foldMap(xi => (xi + 10) ** 2) + exp(-x.foldMap(_ ** 2))

  def brown[F[_]: Foldable, A: Ring : Monoid : NRoot](x: Sized2And[F, A]) =
    (x.a :: x.b :: x.rest.toList).sliding(2).toList.foldMap {
      case List(xi, xi1) => (xi ** 2).fpow((xi1 ** 2) + 1) + (xi1 ** 2).fpow((xi ** 2) + 1)
    }

  def bukin2[A: Field](x: Sized2[A]) = {
    val (x1, x2) = x
    100 * (x2 - 0.01 * (x1 ** 2) + 1) ** 2 + 0.01 * ((x1 + 10) ** 2)
  }

  def bukin2Adapted[A: Field](x: Sized2[A]) = bukin2(x) ** 2

  def bukin4[A: Field : Signed](x: Sized2[A]) = {
    val (x1, x2) = x
    100 * (x2 ** 2) + 0.01 * abs(x1 + 10)
  }

  def bukin6[A: Field : NRoot : Signed](x: Sized2[A]) = {
    val (x1, x2) = x
    100.0 * sqrt(abs(x2 - 0.01 * (x1 ** 2))) + 0.01 * abs(x1 + 10.0)
  }

  def centralTwoPeakTrap[A: Field : Order](x1: Sized1[A]) =
    if      (x1 < 0)   0.0 * x1
    else if (x1 <= 10) (-160.0 / 10.0) * x1
    else if (x1 <= 15) (-160.0 / 5.0) * (15.0 - x1)
    else if (x1 <= 20) (-200.0 / 5.0) * (x1 - 15.0)
    else               (0.0 * x1) - 200.0

  def chichinadze[A: Field : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    val t1 = (x1 ** 2) - (12.0 * x1) + 11.0
    val t2 = 10.0 * cos(pi * (x1 / 2.0)) + 8.0 * sin(5.0 * pi * x1)
    val t3 = ((1.0 / 5.0) ** 0.5) * exp(-0.5 * ((x2 - 0.5) ** 2))

    t1 + t2 - t3
  }

  def chungReynolds[F[_]: Foldable1, A: Ring : Monoid](x: F[A]) = x.foldMap(_ ** 2) ** 2

  def cigar[F[_]: Foldable, A: Field : Monoid](condition: Double = 10e6)(x: Sized2And[F, A]) =
    x.a ** 2 + x.b ** 2 * condition + x.rest.foldMap(_ ** 2) * condition

  def colville[A: Field](x: Sized4[A]) = {
    val (x1, x2, x3, x4) = x
    val t1 = 100.0 * ((x1 - (x2 ** 2)) ** 2) + ((1.0 - x1) ** 2)
    val t2 = 90.0 * ((x4 - x3) ** 2) + ((1.0 - x3) ** 2)
    val t3 = 10.1 * (((x2 - 1.0) ** 2) + ((x4 - 1.0) ** 2))
    val t4 = 19.8 * (x2 - 1.0) * (x4 - 1.0)

    t1 + t2 + t3 + t4
  }

  def cosineMixture[F[_]: Foldable1, A: Field : Trig : Monoid](x: F[A]) =
    -0.1 * x.foldMap(xi => cos(5 * pi * xi)) + x.foldMap(_ ** 2)

  def crossInTray[F[_]: Foldable1, A: NRoot : Signed : Trig : Monoid](x: F[A])(implicit A: Field[A]) = {
    val t1 = x.foldLeft(A.one)((a,c) => a * sin(c))
    val t2 = exp(abs(100.0 - (sqrt(x.foldMap(_ ** 2)) / pi)))

    -0.0001 * ((abs(t1 * t2) + 1.0) ** 0.1)
  }

  def crossLegTable[F[_]: Foldable1, A: Field : NRoot : Signed : Trig : Monoid](x: F[A]) =
    -1.0 / (crossInTray(x) / -0.0001)

  def crossCrowned[F[_]: Foldable1, A: Field : NRoot : Signed : Trig : Monoid](x: F[A]) =
    -crossInTray(x)

  def csendes[F[_]: Foldable1, A: Field : Trig : Monoid](x: F[A]) =
    x.foldMapM { xi =>
      if (xi != 0.0)
        ((xi ** 6) * (2 + sin(1.0 / xi))).some
      else
        none
    }

  def cube[A: Field](x: Sized2[A]) = {
    val (x1, x2) = x
    100 * ((x2 - (x1 ** 3)) ** 2) + ((1.0 - x1) ** 2)
  }

  def damavandi[A: Field : IsReal : Signed : Trig](x: Sized2[A]) = {
    val (x1, x2) = x
    val numer = sin(pi * (x1 - 2)) * sin(pi * (x2 - 2))
    val denum = pi ** 2 * (x1 - 2) * (x2 - 2)
    val factor1 = 1 - (abs(numer / denum) ** 5)
    val factor2 = 2 + (x1 - 7) ** 2 + 2 * (x2 - 7) ** 2

    factor1 * factor2
  }

  def deb1[F[_]: Foldable1, A: Field : Trig : Monoid](x: F[A]) =
    -(1.0 / x.length) * x.foldMap(xi => sin(5 * pi * xi) ** 6)

  def deb3[F[_]: Functor : Foldable1, A: Field : NRoot : Trig : Monoid](x: F[A]) =
    deb1(x.map(xi => (xi ** 0.75) - 0.05))

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

  def differentPowers[F[_]: Foldable, A: NRoot : Ring : Signed : Monoid](x: Sized2And[F, A]) = {
    val n = x.rest.length + 2
    val inner = (x.a :: x.b :: x.rest.toList).zipWithIndex.foldMap {
      case (xi, i) => abs(xi) ** (2 + ((4 * i) / (n - 1)))
    }

    sqrt(inner)
  }

  def discus[F[_]: Foldable, A: Ring : Monoid](x : Sized1And[F, A]) =
    (10 ** 6) * (x.head ** 2) + x.tail.foldMap(_ ** 2)

  def dixonPrice[F[_]: Foldable, A: Ring : Monoid](x: Sized2And[F, A]) = {
      def t(l: (Seq[A], Int)) = l match {
        case (Seq(xi, xi1), i) => (i + 2) * (((2 * (xi1 ** 2)) - xi) ** 2)
      }

      val t1 = ((x.a - 1) ** 2)
      val t2 = (x.a :: x.b :: x.rest.toList).sliding(2).toList.zipWithIndex.foldMap(t)
      t1 + t2
  }

  def dropWave[F[_]: Foldable1, A: Field : NRoot : Trig : Monoid](x: F[A]) = {
    val sumsqr = x.foldMap(_ ** 2)
    -(1 + cos(12 * sqrt(sumsqr))) / (2 + 0.5 * sumsqr)
  }

  // def easom[T: Field : IsReal : Trig](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     Some(-cos(x1) * cos(x2) * exp(-((x1 - pi) ** 2 + (x2 - pi) ** 2)))
  //   case _ => None
  // }

  // def eggCrate[T: Ring : Trig](x: Seq[T]) = spherical(x).map { s =>
  //   spherical(x.map(sin(_))).map(s1 => s + 24 * s1)
  // }.flatten

  // def eggHolder[T: Field : NRoot : Signed : Trig](x: Seq[T]) =
  //   if (x.length >= 2) {
  //     def g(l: Seq[T]) = l match {
  //       case Seq(x1, x2) =>
  //         -(x2 + 47) * sin(sqrt(abs(x2 + (x1 / 2) + 47))) -
  //           x1 * sin(sqrt(abs(x1 - x2 - 47)))
  //     }
  //     Some(x.sliding(2).toList.map(g).qsum)
  //   } else None

  // def elliptic[T: Field](x: Seq[T]) =
  //   if (x.length >= 2)
  //     Some(x.zipWithIndex.map { case (xi, i) =>
  //       (10e6 ** (i / (x.length - 1.0))) * (xi ** 2) }.qsum)
  //   else None

  // def elAttarVidyasagarDutta[T: Ring](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     val t1 = ((x1 ** 2) + x2 - 10) ** 2
  //     val t2 = (x1 + (x2 ** 2) - 7) ** 2
  //     val t3 = ((x1 ** 2) + (x2 ** 3) - 1) ** 2
  //     Some(t1 + t2 + t3)
  //   case _ => None
  // }

  // def exponential1[T: Field : Trig](x: Seq[T]) =
  //   spherical(x).map(s => -exp(-0.5 * s))

  // def exponential2[T: Field : Trig](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     Some((for { i <- 0 to 9
  //       t1 = 1.0 * exp(-i * x1 / 10.0)
  //       t2 = 5.0 * exp(-i * x2 / 10.0)
  //       t3 = 1.0 * exp(-i / 10.0)
  //       t4 = 5.0 * exp(-i)
  //     } yield (t1 - t2 - t3 + t4) ** 2).qsum)
  //   case _ => None
  // }

  // def freudensteinRoth[T: Ring](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     val t1 = (x1 - 13 + ((5 - x2) * x2 - 2) * x2) ** 2
  //     val t2 = (x1 - 29 + ((x2 + 1) * x2 -14) * x2) ** 2
  //     Some(t1 + t2)
  //   case _ => None
  // }

  // def gear[T: Field : IsReal](x: Seq[T]) = x.map(floor(_)) match {
  //   case Seq(x1, x2, x3, x4) =>
  //     val t1 = 1.0 / 6.931
  //     val numer = x1 * x2
  //     val denom = x3 * x4
  //     Some((t1 - (numer / denom)) ** 2)
  //   case _ => None
  // }

  // def giunta[T: Field : Trig](x: Seq[T]) = x match {
  //   case Seq(_, _) =>
  //     val mapped = x.map { xi =>
  //       val factor = (16.0 / 15.0) * xi - 1
  //       val t1 = sin(factor)
  //       val t2 = t1 ** 2
  //       val t3 = (1.0 / 50.0) * sin(4 * factor)
  //       t1 + t2 + t3
  //     }
  //     Some(0.6 + mapped.qsum)
  //   case _ => None
  // }

  // def goldsteinPrice1[T: Ring](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     val t1 = 1 + ((x1 + x2 + 1) ** 2) * (19 - 14 * x1 + 3 * (x1 ** 2) -
  //       14 * x2 + 6 * x1 * x2 + 3 * (x2 ** 2))
  //     val t2 = 30 + ((2 * x1 - 3 * x2) ** 2) * (18 - 32 * x1 + 12 *
  //       (x1 ** 2) + 48 * x2 - 36 * x1 * x2 + 27 * (x2 ** 2))
  //     Some(t1 * t2)
  //   case _ => None
  // }

  // def goldsteinPrice2[T: Field : Trig](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     val t1 = exp(0.5 * (((x1 ** 2) + (x2 ** 2) - 25) ** 2))
  //     val t2 = sin(4 * x1 - 3 * x2) ** 4
  //     val t3 = 0.5 * ((2 * x1 + x2 - 10) ** 2)
  //     Some(t1 + t2 + t3)
  //   case _ => None
  // }

  // def griewank[T: Field : NRoot : Trig](x: Seq[T]) = {
  //   val prod = x.zipWithIndex.map { case (xi, i) =>
  //     cos(xi / sqrt(i + 1))
  //   }.qproduct

  //   spherical(x).map(s => 1 + s * (1.0 / 4000.0) - prod)
  // }

  // def hansen[T: Ring : Trig](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     val t1 = (for (i <- 0 to 4)
  //       yield (i + 1) * cos(i * x1 + i + 1)).qsum
  //     val t2 = (for (j <- 0 to 4)
  //       yield (j + 1) * cos((j + 2) * x2 + j + 1)).qsum
  //     Some(t1 * t2)
  //   case _ => None
  // }

  // def hartman3[T: Field : Trig](x: Seq[T]) =
  //   if (x.length == 3) {

  //     val a = List(
  //       List(3.0, 10.0, 30.0),
  //       List(0.1, 10.0, 35.0),
  //       List(3.0, 10.0, 30.0),
  //       List(0.1, 10.0, 35.0)
  //     )

  //     val c = List(1.0, 1.2, 3.0, 3.2)

  //     val p = List(
  //       List(0.6890, 0.1170, 0.2673),
  //       List(0.4699, 0.4387, 0.7470),
  //       List(0.1091, 0.8732, 0.5547),
  //       List(0.0381, 0.5743, 0.8828)
  //     )

  //     val ts = for { i <- 0 until 4
  //       power = for (j <- 0 until 3) yield a(i)(j) * ((x(j) - p(i)(j)) ** 2)
  //     } yield c(i) * exp(-power.qsum)

  //     Some(-ts.qsum)
  //   } else None

  // def hartman6[T: Field : Trig](x: Seq[T]) =
  //   if (x.length == 6) {

  //     val a = List(
  //       List(10.0, 3.00, 17.0, 3.50, 1.70, 8.00),
  //       List(0.05, 10.0, 17.0, 0.10, 8.00, 14.0),
  //       List(3.00, 3.50, 1.70, 10.0, 17.0, 8.00),
  //       List(17.0, 8.00, 0.05, 10.0, 0.10, 14.0)
  //     )

  //     val c = List(1.0, 1.2, 3.0, 3.2)

  //     val p = List(
  //       List(0.1312, 0.1696, 0.5569, 0.0124, 0.8283, 0.5886),
  //       List(0.2329, 0.4135, 0.8307, 0.3736, 0.1004, 0.9991),
  //       List(0.2348, 0.1451, 0.3522, 0.2883, 0.3047, 0.6650),
  //       List(0.4047, 0.8828, 0.8732, 0.5743, 0.1091, 0.0381)
  //     )

  //     val ts = for { i <- 0 until 4
  //       power = for (j <- 0 until 6) yield a(i)(j) * ((x(j) - p(i)(j)) ** 2)
  //     } yield c(i) * exp(-power.qsum)

  //     Some(-ts.qsum)

  //   } else None

  // def helicalValley[T: Field : NRoot : Order : Trig](x: Seq[T]) = x match {
  //   case Seq(x1, x2, x3) =>

  //     val theta = {
  //       val t  = if (x1 >= (x1 * 0)) 0.0 else 0.5
  //       atan((x2 / x1) + t) / (2 * pi)
  //     }

  //     val t1 = (x2 - 10.0 * theta) ** 2
  //     val t2 = sqrt((x1 ** 2) + (x2 ** 2)) - 1.0
  //     val t3 = x3 ** 2

  //     Some(100.0 * (t1 + t2) + t3)

  //   case _ => None
  // }

  // def himmelblau[T: Ring](x: Seq[T]) = x match {
  //   case Seq(x1, x2) => Some((x1 ** 2 + x2 - 11) ** 2 + (x1 + x2 ** 2 - 7) ** 2)
  //   case _           => None
  // }

  // def holzman[T: Field : Trig](x: Seq[T])(implicit nr: NRoot[T]) = x match {
  //   case Seq(x1, x2, x3) =>
  //     val ts = for { i <- 0 to 99
  //       ui    = 25.0 + ((-50.0 * log(0.01 * (i + 1))) ** (2.0 / 3.0))
  //       t1 = exp(-nr.fpow(ui - x2, x3) / x1)
  //       t2 = 0.01 * (i + 1)
  //     } yield (t1 - t2)
  //     Some(ts.qsum)
  //   case _ => None
  // }

  // def hosaki[T: Field : Trig](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     val t1 = 1 - 8 * x1 + 7 * (x1 ** 2)
  //     val t2 = (7.0 / 3.0) * (x1 ** 3)
  //     val t3 = (1.0 / 4.0) * (x1 ** 4)
  //     val t4 = (x2 ** 2) * exp(-x2)
  //     Some((t1 - t2 + t3) * t4)
  //   case _ => None
  // }

  // def hyperEllipsoid[T: Ring](x: Seq[T]) =
  //   Some(x.zipWithIndex.map { case (xi, i) => i * (xi ** 2) }.qsum)

  // def hyperEllipsoidRotated[T: Ring](x: Seq[T]) = {
  //   val values = for (i <- 1 to x.length) yield x take i
  //   Some(values.map(spherical(_)).flatten.qsum)
  // }

  // def infinity[T: Field : Trig](x: Seq[T]) =
  //   if (x.forall(_ != 0.0))
  //     Some(x.map(xi => (xi ** 6) * (sin(1.0 / xi) + 2)).qsum)
  //   else None

  // def jennrichSampson[T: Ring : Trig](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     val ts = for { i <- 1 to 10
  //       t1 = 2 + 2 * i
  //       t2 = exp(i * x1) + exp(i * x2)
  //     } yield (t1 - t2) ** 2
  //     Some(ts.qsum)
  //   case _ => None
  // }

  // def judge[T: Field](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     val A = List(
  //       4.284, 4.149, 3.877, 0.533, 2.211,
  //       2.389, 2.145, 3.231, 1.998, 1.379,
  //       2.106, 1.428, 1.011, 2.179, 2.858,
  //       1.388, 1.651, 1.593, 1.046, 2.152
  //     )

  //     val B = List(
  //       0.286, 0.973, 0.384, 0.276, 0.973,
  //       0.543, 0.957, 0.948, 0.543, 0.797,
  //       0.936, 0.889, 0.006, 0.828, 0.399,
  //       0.617, 0.939, 0.784, 0.072, 0.889
  //     )

  //     val C = List(
  //       0.645, 0.585, 0.310, 0.058, 0.455,
  //       0.779, 0.259, 0.202, 0.028, 0.099,
  //       0.142, 0.296, 0.175, 0.180, 0.842,
  //       0.039, 0.103, 0.620, 0.158, 0.704
  //     )

  //     val mappedB = B.map(_ * x2)
  //     val mappedC = C.map(_ * (x2 ** 2))

  //     val t1 = mappedB.zip(mappedC).map { case (ai, bi) => ai + bi }
  //     val t2 = t1.map(_ + x1)
  //     val t3 = t2.zip(A).map { case (t2, ai) => t2 - ai }
  //     Some(t3.map(_ ** 2).qsum)

  //   case _ => None
  // }

  // def katsuura[T: Field : IsReal : NRoot](x: Seq[T]) = {
  //   val n = x.length

  //   val ts = x.zipWithIndex.map { case (xi, i) =>
  //     val t1 = i + 1
  //     val d = 32
  //     val t2 = for (k <- 1 to d) yield floor((2 ** k) * xi) * (1.0 / (2 ** k))
  //     1 + t1 * t2.qsum
  //   }

  //   Some(ts.qproduct)
  // }

  // def kowalik[T: Field](x: Seq[T]) = x match {
  //   case Seq(x1, x2, x3, x4) =>
  //     val b = List(
  //       4.0, 2.0, 1.0, 0.5, 0.25, 1.0 / 6.0, 0.125,
  //       0.1, 1.0 / 12.0, 1.0 / 14.0, 0.0625
  //     )

  //     val a = List(
  //       0.1957, 0.1947, 0.1735, 0.1600, 0.0844, 0.0627,
  //       0.0456, 0.0342, 0.0323, 0.0235, 0.0246
  //     )

  //     val ts = a.zip(b).map { case (ai, bi) =>
  //       val numer = x1 * ((bi ** 2) + bi * x2)
  //       val denom = (bi ** 2) + bi * x3 + x4
  //       ai - (numer / denom)
  //     }
  //     Some(ts.map(_ ** 2).qsum)
  //   case _ => None
  // }

  // def leon[T: Ring](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     val t1 = 100 * ((x2 - (x1 ** 2)) ** 2)
  //     val t2 = (1 - x1) ** 2
  //     Some(t1 + t2)
  //   case _ => None
  // }

  // def levy3[T: Field : Trig](x: Seq[T]) =
  //   if (x.length >= 2) {
  //     def y(xi: T): T = 1 + (xi - 1) / 4.0

  //     val t1 = sin(pi * y(x.head)) ** 2

  //     val t2 = x.sliding(2).toList.map {
  //       case List(xi, xi1) =>
  //         ((y(xi) - 1) ** 2) * (1 + 10 * ((pi * y(xi1) ** 2)))
  //     }.qsum

  //     val t3 = (y(x.last) - 1) ** 2

  //     Some(t1 + t2 + t3)
  //   } else None

  // def levy5[T: Field : Trig](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     val t1 = (for (i <- 1 to 5) yield i * cos((i - 1) * x1 + i)).qsum
  //     val t2 = (for (j <- 1 to 5) yield j * cos((j + 1) * x2 + j)).qsum
  //     val t3 = (x1 + 1.42513) ** 2
  //     val t4 = (x2 + 0.80032) ** 2
  //     Some(t1 * t2 + t3 + t4)
  //   case _ => None
  // }

  // def levy13[T: Field : Trig](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     val t1 = ((x1 - 1) ** 2) * ((sin(3 * pi * x2) ** 2) + 1)
  //     val t2 = ((x2 - 1) ** 2) * ((sin(2 * pi * x2) ** 2) + 1)
  //     val t3 = sin(3 * pi * x1) ** 2
  //     Some(t1 + t2 + t3)
  //   case _ => None
  // }

  // def levyMontalvo2[T: Field : Trig](x: Seq[T]) =
  //   if (x.length >= 2) {
  //     val t1 = sin(3 * pi * x.head) ** 2

  //     def t(a: Seq[T]) = a match {
  //       case Seq(xi, xi1) =>
  //         ((xi - 1) ** 2) * ((sin(3 * pi * xi1) ** 2) + 1 )
  //     }

  //     val t2 = x.sliding(2).toList.map(t).qsum

  //     val t3 = ((x.last - 1) ** 2) * ((sin(2 * pi * x.last) ** 2) + 1)

  //     Some(0.1 * (t1 + t2 + t3))
  //   } else None

  // def matyas[T: Field : IsReal](x: Seq[T]) = x match {
  //   case Seq(x1, x2) => Some(0.26 * (x1 ** 2 + x2 ** 2) - 0.48 * x1 * x2)
  //   case _           => None
  // }

  // def maximum[T: Ordering](x: Seq[T]) = x match {
  //   case Seq() => None
  //   case _     => Some(x.max)
  // }

  // def mcCormick[T: Field : Trig](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     val t1 = sin(x1 + x2) + ((x1 - x2) ** 2)
  //     val t2 = -1.5 * x1 + 2.5 * x2 + 1
  //     Some(t1 + t2)
  //   case _ => None
  // }

  // def michalewicz[T: Field : IsReal : NRoot : Trig](x: Seq[T]) = {
  //   val m = 10.0
  //   Some(-x.zipWithIndex.map { case (xi, i) =>
  //     sin(xi) * (sin(((i + 1) * (xi ** 2)) / pi) ** (2 * m))
  //   }.qsum)
  // }

  // def mieleCantrell[T: Field : Trig](x: Seq[T]) = x match {
  //   case Seq(x1, x2, x3, x4) =>
  //     val t1 = (exp(-x1) - x2) ** 4
  //     val t2 = 100 * ((x2 - x3) ** 6)
  //     val t3 = tan(x3 - x4) ** 4
  //     val t4 = x1 ** 8
  //     Some(t1 + t2 + t3 + t4)
  //   case _ => None
  // }

  // def minimum[T: Ordering](x: Seq[T]) = x match {
  //   case Seq() => None
  //   case _     => Some(x.min)
  // }

  // def mishra1[T: Ring](x: Seq[T])(implicit nr: NRoot[T]) =
  //   if (!x.isEmpty) {
  //     val sum = x.init.qsum
  //     val n = x.length
  //     Some(nr.fpow(1 + n - sum, n - sum))
  //   } else None

  // def mishra2[T: Field](x: Seq[T])(implicit nr: NRoot[T]) =
  //   if (x.length >= 2) {

  //     def t(a: Seq[T]) = a match {
  //       case Seq(xi, xi1) => 0.5 * (xi + xi1)
  //     }

  //     val sum = x.sliding(2).toList.map(t).qsum
  //     val n = x.length

  //     Some(nr.fpow(1 + n - sum, n - sum))
  //   } else None

  // def mishra3[T: Field : NRoot : Signed : Trig](x: Seq[T]) = {
  //   val sum = spherical(x)
  //   sum.map(s => sqrt(abs(cos(sqrt(s)))) + 0.01 * x.qsum)
  // }

  // def mishra5[T: Field : Trig](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     val t1 = sin((cos(x1) + cos(x2)) ** 2) ** 2
  //     val t2 = cos((sin(x1) + sin(x2)) ** 2) ** 2
  //     val t3 = 0.01 * (x1 + x2)
  //     Some(((t1 + t2 + x1) ** 2) + t3)
  //   case _ => None
  // }

  // def mishra8[T: Field : Signed](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     val coX1 = List(1, -20, 180, -960, 3360, -8064,
  //       13340, -15360, 11520, -5120, 2624)
  //     val coX2 = List(1, 12, 54, 108, 81)

  //     val t1 = abs(coX1.zipWithIndex.map {
  //       case (ci, i) => ci * (x1 ** (coX1.length - 1 - i))
  //     }.qsum)

  //     val t2 = abs(coX2.zipWithIndex.map {
  //       case (ci, i) => ci * (x2 ** (coX2.length - 1 - i))
  //     }.qsum)

  //     Some(0.001 * ((t1 * t2) ** 2))
  //   case _ => None
  // }

  // def mishra11[T: Field : NRoot : Signed](x: Seq[T]) = {
  //   val n = x.length
  //   val t1 = (1.0 / n) * x.map(abs(_)).qsum
  //   val t2 = x.map(abs(_)).qproduct ** (1.0 / n)
  //   Some((t1 - t2) ** 2)
  // }

  // def multiModal[T: Field : Signed](x: Seq[T]) =
  //   Some(x.map(abs(_)).qproduct * x.map(abs(_)).qsum)

  // // def nastyBenchmark[T: Field](x: Seq[T]) =
  // //   Some(x.zipWithIndex.map { case(xi, i) => (xi - (i + 1)) ** 2 })

  // // def oddSquare[T: Field : Ordering : Trig](x: Seq[T]) = {
  // //   val b = List(
  // //     1.0, 1.3, 0.8, -0.4, -1.3,
  // //     1.6, -0.2, -0.6, 0.5, 1.4,
  // //     1.0, 1.3, 0.8, -0.4, -1.3,
  // //     1.6, -0.2, -0.6, 0.5, 1.4
  // //   )
  // // }

  // def parsopoulus[T: Field : Trig](x: Seq[T]) = x match {
  //   case Seq(x1, x2) => Some((cos(x1) ** 2) + (sin(x2) ** 2))
  //   case _ => None
  // }

  // // def paviani[T: Field : NRoot : Trig](x: Seq[T]) =
  // //   if (x.length == 10) {
  // //     val t1 = x.map(xi => (log(10 - xi) ** 2) + (log(xi - 2) ** 2)).qsum
  // //     val t2 = x.map(xi => xi ** 10).qproduct ** 0.2
  // //     println("Term2: %f".format(t2))
  // //     Some(t1 - t2)
  // //   } else None

  // def penalty1[T: Field : Order : Trig](x: Seq[T]) =
  //   if (x.length >= 2) {
  //     def u(xi: T, a: Int, k: Int, m: Int) =
  //       if (xi > a) k * ((xi - a) ** m)
  //       else if (xi < -a) k * ((-xi - a) ** m)
  //       else 0.0 * xi

  //     def yi(xi: T) = 1 + ((xi + 1.0) / 4.0)

  //     val term1 = 10 * (sin(pi * yi(x.head)) ** 2)
  //     val term2 = x.sliding(2).toList.map {
  //       case Seq(xi, xi1) =>
  //         val t1 = (yi(xi) - 1.0) ** 2
  //         val t2 = 1.0 + 10.0 * (sin(pi * yi(xi1)) ** 2)
  //         t1 * t2
  //     }.qsum
  //     val term3 = (yi(x.last) - 1.0) ** 2
  //     val term4 = x.map(xi => u(xi, 10, 100, 4)).qsum

  //     Some((pi / 30.0) * (term1 + term2 + term3) + term4)
  //   } else None

  // def penalty2[T: Field : Order : Trig](x: Seq[T]) =
  //   if (x.length >= 2) {
  //     def u(xi: T, a: Int, k: Int, m: Int) =
  //       if (xi > a) k * ((xi - a) ** m)
  //       else if (xi < -a) k * ((-xi - a) ** m)
  //       else 0.0 * xi

  //     val term1 = sin(3.0 * pi * x.head) ** 2
  //     val term2 = x.sliding(2).toList.map {
  //       case Seq(xi, xi1) =>
  //         val t1 = (xi - 1.0) ** 2
  //         val t2 = 1.0 + (sin(3.0 * pi * xi1) ** 2)
  //         t1 * t2
  //     }.qsum
  //     val term3 = ((x.last - 1.0) ** 2) * (1.0 + sin(2.0 * pi * x.last) ** 2)
  //     val term4 = x.map(xi => u(xi, 5, 100, 4)).qsum

  //     Some(0.1 * (term1 + term2 + term3) + term4)
  //   } else None

  // def penHolder[T: Field : NRoot : Signed : Trig](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     val t1 = abs(1 - (sqrt((x1 ** 2) + (x2 ** 2)) / pi))
  //     val t2 = cos(x1) * cos(x2)
  //     val expon = abs(exp(t1) * t2) ** -1
  //     Some(-exp(-expon))
  //   case _ => None
  // }

  // def periodic[T: Field : Trig](x: Seq[T]) = {
  //   val t1 = x.map(xi => sin(xi) ** 2).qsum
  //   val t2 = 0.1 * exp(-x.map(_ ** 2).qsum)
  //   Some(1 + t1 - t2)
  // }


  // def powell[T: Field](x: Seq[T]) = x match {
  //   case Seq(x1, x2, x3, x4) =>
  //     val t1 = (x3 + 10 * x1) ** 2
  //     val t2 = 5.0 * ((x2 - x4) ** 2)
  //     val t3 = (x1 - 2 * x2) ** 4
  //     val t4 = 10.0 * ((x3 - x4) ** 4)
  //     Some(t1 + t2 + t3 + t4)
  //   case _ => None
  // }

  // def powellSum[T: Ring : Signed](x: Seq[T]) =
  //   Some(x.zipWithIndex.map {
  //     case (xi, i) => abs(xi) ** (i + 1)
  //   }.qsum)

  // def powerSum[T: Ring](x: Seq[T]) =
  //   if (x.length == 4) {
  //     val b = List(8, 18, 44, 114)

  //     val terms = b.zipWithIndex.map {
  //       case (bk, i) =>
  //         val k = i + 1
  //         val t = x.map(xi => xi ** k).qsum
  //         (t - bk) ** 2
  //     }

  //     Some(terms.qsum)
  //   } else None

  // def price1[T: Ring : Signed](x: Seq[T]) =
  //   Some(x.map(xi => (abs(xi) - 5) ** 2).qsum)

  // def price2[T: Field : Trig](x: Seq[T]) =
  //   Some(1.0 + x.map(xi => sin(xi) ** 2).qsum - 0.1 * exp(-x.map(_ ** 2).qsum))

  // def price3[T: Field](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     val t1 = 100 * ((x2 - (x1 ** 2)) ** 2)
  //     val t2 = 6 * ((6.4 * ((x2 - 0.5) ** 2) - x1 - 0.6) ** 2)
  //     Some(t1 + t2)
  //   case _ => None
  // }

  // def qing[T: Field](x: Seq[T]) =
  //   Some(x.zipWithIndex.map {
  //     case (xi, i) => ((xi ** 2) - (i + 1.0)) ** 2
  //   }.qsum)

  // def quadratic[T: Field](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     val t1 = -3803.84 - 138.08 * x1
  //     val t2 = -232.92 * x2 + 128.08 * (x1 ** 2)
  //     val t3 = 203.64 * (x2 ** 2) + 182.25 * x1 * x2
  //     Some(t1 + t2 + t3)
  //   case _ => None
  // }

  // def quadric[T: Ring](x: Seq[T]) = {
  //   val terms = for (i <- 1 to x.length) yield (x take i).qsum ** 2
  //   Some(terms.qsum)
  // }

  // def quintic[T: Field : Signed](x: Seq[T]) = {
  //   val terms = x.map { xi =>
  //     (xi ** 5) - 3 * (xi ** 4) + 4 * (xi ** 3) + 2 * (xi ** 2) - 10.0 * xi - 4
  //   }

  //   Some(abs(terms.qsum))
  // }

  // def rastrigin[T: Field : IsReal : Trig](x: Seq[T]) =
  //   Some(10 * x.size + x.map(xi => xi ** 2 - 10 * cos(2 * pi * xi)).qsum)


  // // def ripple1[T: Field : Trig](x: Seq[T]) = {
  // //   val terms = x.map { xi =>
  // //     val t1 = -2 * (log(((xi - 0.1) / 0.8) ** 2) / log(2))
  // //     val t2 = sin(5 * pi * xi) ** 6
  // //     val t3 = 0.1 * (cos(500 * pi * xi) ** 2)
  // //     -exp(t1 * (t2 + t3))
  // //   }
  // //   Some(terms.qsum)
  // // }

  // def rosenbrock[T: Ring](x: Seq[T]) =
  //   Some((0 until x.length - 1).map(i => {
  //     val t1 = 100 * (x(i + 1) - x(1) ** 2) ** 2
  //     val t2 = (x(1) - 1) ** 2
  //     t1 + t2
  //   }).qsum)

  // def rotatedEllipse1[T: Field](x: Seq[T]) =
  //   if (x.length >= 2) {
  //     def g(l: Seq[T]) = l match {
  //       case Seq(x1, x2) =>
  //         (7 * (x1 ** 2)) - (6 * sqrt(3.0) * x1 * x2) + (13 * (x2 ** 2))
  //     }
  //     Some(x.sliding(2).toList.map(g(_)).qsum)
  //   } else None

  // def rotatedEllipse2[T: Ring](x: Seq[T]) =
  //   if (x.length >= 2) {
  //     def g(l: Seq[T]) = l match {
  //       case Seq(x1, x2) => (x1 ** 2) - (x1 * x2) + (x2 ** 2)
  //     }
  //     Some(x.sliding(2).toList.map(g(_)).qsum)
  //   } else None

  // // def rump[T: Field](x: Seq[T]) =
  // //   if (x.length >= 2) {
  // //     val terms = x.sliding(2).toList.map {
  // //       case Seq(xi, xi1) =>
  // //         val t1 = (333.75 - xi ** 2) * (xi1 ** 6)
  // //         val t2 = (xi ** 2) * (11 * (xi ** 2) * (xi1 ** 2) -
  // //           121 * (xi1 ** 4) - 2)
  // //         val t3 = 5.5 * (xi1 ** 8)
  // //         val t4 = xi / (2 * xi1)
  // //         t1 + t2 + t3 + t4
  // //     }

  // //     Some(terms.qsum)
  // //   } else None

  // def salomon[T: Field : NRoot : Trig](x: Seq[T]) =
  //   spherical(x).map(sum => -cos(2 * pi * sqrt(sum)) + (0.1 * sqrt(sum)) + 1)

  // // def schaffer1[T: Field : Trig](x: Seq[T]) =
  // //   if (x.length >= 2) {
  // //     val terms = x.sliding(2).toList.map {
  // //       case Seq(xi, xi1) =>
  // //         val t1 = (xi ** 2) - (xi1 ** 2)
  // //         val t2 = (xi ** 2) + (xi1 ** 2)
  // //         val t3 = (sin((t1) ** 2) ** 2) - 0.5
  // //         val t4 = (1 + 0.001 * t2) ** 2
  // //         0.5 + (t3 / t4)
  // //     }

  // //     Some(terms.qsum)
  // //   } else None

  // // def schaffer2[T: Field : Signed : Trig](x: Seq[T]) =
  // //   if (x.length >= 2) {
  // //     val terms = x.sliding(2).toList.map {
  // //       case Seq(xi, xi1) =>
  // //         val t1 = (xi ** 2) + (xi1 ** 2)
  // //         val t2 = (sin((t1) ** 2) ** 2) - 0.5
  // //         val t3 = (1 + 0.001 * t1) ** 2
  // //         0.5 + (t2 / t3)
  // //     }

  // //     Some(terms.qsum)
  // //   } else None
  // // def schaffer2[T: Field : NRoot](x: Seq[T]) = x match {
  // //   case Seq(_, _) => spherical(x).map(sum =>
  // //     (sum ** 0.25) * (50 * (sum ** 0.1) + 1))
  // //   case _ => None
  // // }

  // // def schmidtVetters[T: Field : Trig](x: Seq[T]) = x match {
  // //   case Seq(x1, x2, x3) =>
  // //     val t1 = 1.0 / (1.0 + (x1 - x2) ** 2)
  // //     val t2 = sin((pi * x2 + x3) / 2.0)
  // //     val t3 = exp((((x1 + x2) / x2) - 2.0) ** 2)
  // //     Some(t1 + t2 + t3)
  // //   case _ => None
  // // }

  // def schwefel1[T: Field : NRoot](x: Seq[T]) = {
  //   val alpha = sqrt(pi)
  //   spherical(x).map(_ ** alpha)
  // }

  // def schwefel12[T: Ring](x: Seq[T]) =
  //   Some(x.zipWithIndex.map { case (xi, i) => x.take(i + 1).qsum ** 2 }.qsum)

  // def schwefel221[T: Ordering : Signed](x: Seq[T]) = maximum(x.map(abs(_)))

  // def schwefel222[T: Ring : Signed](x: Seq[T]) =
  //   Some(x.map(abs(_)).qsum + x.map(abs(_)).qproduct)

  // def schwefel223[T: Ring](x: Seq[T]) = Some(x.map(_ ** 10).qsum)

  // def schwefel226[T: Field : NRoot : Signed : Trig](x: Seq[T]) =
  //   Some(418.9829 * x.length - x.map(xi => xi * sin(sqrt(abs(xi)))).qsum)

  // def schwefel24[T: Field](x: Seq[T]) = x match {
  //   case x1 :: _ =>
  //     val terms = x.map(xi => ((xi - 1) ** 2) + ((x1 - xi) ** 2))
  //     Some(terms.qsum)
  //   case _ => None
  // }

  // def schwefel26[T: Order : Ring : Signed](x: Seq[T]) = x match {
  //   case Seq(x1, x2) => Some(max(abs(x1 + 2 * x2 - 7), abs(2 * x1 + x2 - 5)))
  //   case _ => None
  // }

  // def shubert[T: Ring : Trig](x: Seq[T]) =
  //   if (x.length == 2)
  //     Some(x.map(xi => (1 to 5).map(j => j * cos((j + 1) * xi + j)).qsum).qproduct)
  //   else None

  // def sixHumpCamelback[T: Field](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     val tX1 = 4 * (x1 ** 2) - 2.1 * (x1 ** 4) + (1.0 / 3.0) * (x1 ** 6)
  //     val tX2 = x1 * x2 - 4 * (x2 ** 2) + 4 * (x2 ** 4)
  //     Some(tX1 + tX2)
  //   case _ => None
  // }

  // def spherical[T: Ring](x: Seq[T]) = Some(x.map(_ ** 2).qsum)

  // def step1[T: IsReal : Ring : Signed](x: Seq[T]) =
  //   Some(x.map(xi => floor(abs(xi))).qsum)

  // def step2[T: Field : IsReal](x: Seq[T]) =
  //   Some(x.map(xi => (floor(xi) + 0.5) ** 2).qsum)

  // def step3[T: IsReal : Ring](x: Seq[T]) =
  //   Some(x.map(xi => floor(xi ** 2)).qsum)

  // def sumSquares[T: Ring](x: Seq[T]) =
  //   Some(x.zipWithIndex.map { case (xi, i) => (i + 1) * (xi ** 2) }.qsum)

  // def sumDifferentPowers[T: Ring : Signed](x: Seq[T]) =
  //   Some(x.zipWithIndex.map { case (xi, i) => abs(xi) ** (i + 2) }.qsum)

  // def styblinksiTang[T: Field](x: Seq[T]) = {
  //   val terms = x.map(xi => (xi ** 4) - 16 * (xi ** 2) + 5 * xi)
  //   Some(0.5 * terms.qsum)
  // }

  // def threeHumpCamelback[T: Field : IsReal](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     Some(2 * (x1 ** 2) - 1.05 * (x1 ** 4) +
  //       ((x1 ** 6) / 6) + x1 * x2 + x2 ** 2)
  //   case _ => None
  // }

  // def trecanni[T: Ring](x: Seq[T]) = x match {
  //   case Seq(x1, x2) =>
  //     Some(x1 ** 4 + 4 * (x1 ** 3) + 4 * (x1 ** 2) + (x2 ** 2))
  //   case _ => None
  // }

  // def vincent[T: Field : Trig](x: Seq[T]) =
  //   Some(-x.map(xi => sin(10.0 * log(xi))).qsum)

  // def weierstrass[T: Field : Trig](x: Seq[T]) = {
  //   val a = 0.5
  //   val b = 3.0
  //   val kmax = 20
  //   val constant = (for { k <- 0 to kmax
  //     t1 = a ** k
  //     t2 = cos(2 * pi * (b ** k) * 0.5)
  //   } yield t1 * t2).qsum

  //   val factor1 = x.map(xi => (for { k <- 0 to kmax
  //     t1 = a ** k
  //     t2 = cos(2 * pi * (b ** k) * (xi + 0.5))
  //   } yield t1 * t2).qsum).qsum

  //   factor1 - x.length * constant
  // }

  // def wolfe[T: Field : NRoot](x: Seq[T]) = x match {
  //   case Seq(x1, x2, x3) =>
  //     Some((4.0 / 3.0) * (((x1 ** 2) + (x2 ** 2)) ** 0.75) + x3)
  //   case _ => None
  // }

  // def wood[T: Field](x: Seq[T]) = x match {
  //   case Seq(x1, x2, x3, x4) =>
  //     val t1 = 100 * (((x1 ** 2) - x2) ** 2)
  //     val t2 = (x1 - 1) ** 2 + (x3 - 1) ** 2
  //     val t3 = 90 * ((x3 ** 2 - x4) ** 2)
  //     val t4 = 10.1 * ((x2 - 1) ** 2)
  //     val t5 = (x4 - 1) ** 2 + 19.8 * (x2 - 1) * (x4 - 1)
  //     Some(t1 + t2 + t3 + t4 + t4)
  //   case _ => None
  // }

  // def yaoLiu[T: Ordering : Signed](x: Seq[T]) = maximum(x.map(abs(_)))

  // def zakharov[T: Field : IsReal](x: Seq[T]) = {
  //   val t = x.zipWithIndex.map { case (xi, i) => 0.5 * i * xi }.qsum
  //   spherical(x).map(sum => sum + t ** 2 + t ** 4)
  // }

  // def zettle[T: Field](x: Seq[T]) = x match {
  //   case Seq(x1, x2) => Some((x1 ** 2 + x2 ** 2 - 2 * x1) ** 2 + x1 / 4.0)
  //   case _           => None
  // }
}
