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

object Benchmarks {

  type Sized1And[F[_], A] = OneAnd[F, A]
  final case class Sized2And[F[_], A](a: A, b: A, rest: F[A])

  def absoluteValue[F[_]: Foldable1, A: Signed: Monoid](x: F[A]) =
    x.foldMap(abs(_))

  def ackley[F[_]: Foldable1, A: Field : IsReal : NRoot : Trig : Monoid](x: F[A]) = {
    val n = x.length
    val sumcos = x.foldMap(x => cos(2 * pi * x))
    val sumsqr = x.foldMap(_ ** 2)

    -20 * exp(-0.2 * sqrt(sumsqr / n)) - exp(sumcos / n) + 20 + e
  }

  def alpine1[F[_]: Foldable1, A: Field : Signed : Trig : Monoid](x: F[A]) =
    x.foldMap(xi => abs((xi * sin(xi)) + (0.1 * xi)))

  def arithmeticMean[F[_]: Foldable1, A: NRoot : Monoid](x: F[A])(implicit A: Field[A]) = {
    val n = x.length
    val avg = x.fold / n
    val rootProd = x.foldLeft(A.one)(_ * _) ** (1.0 / n)

    (avg - rootProd) ** 2
  }

  def brent[F[_]: Foldable1, A: Ring : Trig : Monoid](x: F[A]) =
    x.foldMap(xi => (xi + 10) ** 2) + exp(-x.foldMap(_ ** 2))

  def brown[F[_]: Foldable, A: Ring : Monoid : NRoot](x: Sized2And[F, A]) =
    (x.a :: x.b :: x.rest.toList).sliding(2).toList.foldMap {
      case List(xi, xi1) => (xi ** 2).fpow((xi1 ** 2) + 1) + (xi1 ** 2).fpow((xi ** 2) + 1)
    }

  def chungReynolds[F[_]: Foldable1, A: Ring : Monoid](x: F[A]) = x.foldMap(_ ** 2) ** 2

  def cigar[F[_]: Foldable, A: Field : Monoid](condition: Double = 10e6)(x: Sized2And[F, A]) =
    x.a ** 2 + x.b ** 2 * condition + x.rest.foldMap(_ ** 2) * condition

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

  def deb1[F[_]: Foldable1, A: Field : Trig : Monoid](x: F[A]) =
    -(1.0 / x.length) * x.foldMap(xi => sin(5 * pi * xi) ** 6)

  def deb3[F[_]: Functor : Foldable1, A: Field : NRoot : Trig : Monoid](x: F[A]) =
    deb1(x.map(xi => (xi ** 0.75) - 0.05))

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

  def eggCrate[F[_]: Foldable1, A: Ring : Trig : Monoid](x: F[A]) =
    x.foldMap(_ ** 2) + 24 * x.foldMap(sin(_) ** 2)

  def eggHolder[F[_]: Foldable, A: Field : NRoot : Signed : Trig : Monoid](x: Sized2And[F, A]) = {
    def g(l: Seq[A]) = l match {
      case Seq(x1, x2) =>
        -(x2 + 47) * sin(sqrt(abs(x2 + (x1 / 2) + 47))) -
        x1 * sin(sqrt(abs(x1 - x2 - 47)))
    }
    (x.a :: x.b :: x.rest.toList).sliding(2).toList.foldMap(g)
  }

  def elliptic[F[_]: Foldable, A: Field : Monoid](x: Sized2And[F, A]) = {
    val n = x.rest.length + 2
      (x.a :: x.b :: x.rest.toList).zipWithIndex.foldMap {
        case (xi, i) => (10e6 ** (i / (n - 1.0))) * (xi ** 2)
      }
  }

  def exponential1[F[_]: Foldable1, A: Field : Trig : Monoid](x: F[A]) =
    -exp(-0.5 * x.foldMap(_ ** 2))

  def griewank[F[_]: Foldable1, A: Field : NRoot : Trig : Monoid](x: F[A]) = {
    val prod = x.toList.zipWithIndex.map { case (xi, i) =>
      cos(xi / sqrt(i + 1.0))
    }.qproduct

    1 + x.foldMap(_ ** 2) * (1.0 / 4000.0) - prod
  }

  def hyperEllipsoid[F[_]: Foldable1, A: Ring](x: F[A]) =
    x.toList.zipWithIndex.map { case (xi, i) => i * (xi ** 2) }.qsum

  def hyperEllipsoidRotated[F[_]: Foldable1, A: Ring : Monoid](x: F[A]) = {
    val y = x.toList
    val values = (1 to x.length).toList.map(y take _)
    values.foldMap(_.foldMap(xi => xi ** 2))
  }

  def katsuura[F[_]: Foldable1, A: Field : IsReal : NRoot](x: F[A]) = {
    val n = x.length

    val ts = x.toList.zipWithIndex.map { case (xi, i) =>
      val t1 = i + 1
      val d = 32
      val t2 = for (k <- 1 to d) yield floor((2 ** k) * xi) * (1.0 / (2 ** k))
      1 + t1 * t2.qsum
    }

    ts.qproduct
  }

  def levy3[F[_]: Foldable, A: Field : Trig](x: Sized2And[F, A]) = {
    def y(xi: A): A = 1 + (xi - 1) / 4.0

    val t1 = sin(pi * y(x.a)) ** 2
    val t2 = (x.a :: x.b :: x.rest.toList).sliding(2).toList.map {
      case List(xi, xi1) =>
        ((y(xi) - 1) ** 2) * (1 + 10 * ((pi * y(xi1) ** 2)))
    }.qsum
    val t3 = (y(x.rest.toList.lastOption.getOrElse(x.b)) - 1) ** 2

    t1 + t2 + t3
  }

  def levyMontalvo2[F[_]: Foldable, A: Field : Trig](x: Sized2And[F, A]) = {
    val t1 = sin(3 * pi * x.a) ** 2

    def t(a: Seq[A]) = a match {
      case Seq(xi, xi1) =>
        ((xi - 1) ** 2) * ((sin(3 * pi * xi1) ** 2) + 1 )
    }

    val t2 = (x.a :: x.b :: x.rest.toList).sliding(2).toList.map(t).qsum
    val last = x.rest.toList.lastOption.getOrElse(x.b)
    val t3 = ((last - 1) ** 2) * ((sin(2 * pi * last) ** 2) + 1)

    0.1 * (t1 + t2 + t3)
  }

  def maximum[F[_]: Foldable1, A: scalaz.Order](x: F[A]) =
    x.maximum1

  def michalewicz[F[_]: Foldable1, A: Field : IsReal : NRoot : Trig](m: Double = 10.0)(x: F[A]) =
    -x.toList.zipWithIndex.map { case (xi, i) =>
      sin(xi) * (sin(((i + 1) * (xi ** 2)) / pi) ** (2 * m))
    }.qsum

  def minimum[F[_]: Foldable1, A: scalaz.Order](x: F[A]) =
    x.minimum1

  def mishra1[F[_]: Foldable1, A: Ring : NRoot](x: F[A]) = {
    val sum = x.toList.init.qsum
    val n = x.length
    (1 + n - sum).fpow(n - sum)
  }

  def mishra2[F[_]: Foldable, A: Field : NRoot](x: Sized2And[F, A]) = {
    def t(a: Seq[A]) = a match {
      case Seq(xi, xi1) => 0.5 * (xi + xi1)
    }

    val sum = (x.a :: x.b :: x.rest.toList).sliding(2).toList.map(t).qsum
    val n = x.rest.length + 2

    (1 + n - sum).fpow(n - sum)
  }

  def mishra11[F[_]: Foldable1, A: NRoot : Signed : Monoid](x: F[A])(implicit A: Field[A]) = {
    val n = x.length
    val t1 = (1.0 / n) * x.foldMap(abs(_))
    val t2 = x.foldLeft(A.one)(_ * abs(_)) ** (1.0 / n)
    (t1 - t2) ** 2
  }

  def multiModal[F[_]: Foldable1, A: Signed : Monoid](x: F[A])(implicit A: Field[A]) =
    x.foldLeft(A.one)(_ * abs(_)) * x.foldMap(abs(_))

  def pathological[F[_]: Foldable, A: Field : NRoot : Trig : Monoid](x: Sized2And[F, A]) =
    (x.a :: x.b :: x.rest.toList).sliding(2).toList.foldMap {
      case Seq(xi, xi1) =>
        val numer = sin(sqrt(100.0 * (xi ** 2) + (xi1 ** 2))) ** 2 - 0.5
        val denom = 1.0 + 0.001 * (((xi ** 2) - 2.0 * xi * xi1 + (xi1 ** 2)) ** 2)
        0.5 + numer / denom
    }

  def penalty1[F[_]: Foldable, A: Field : Order : Trig : Monoid](x: Sized2And[F, A]) = {
    def u(xi: A, a: Int, k: Int, m: Int) =
      if (xi > a) k * ((xi - a) ** m)
      else if (xi < -a) k * ((-xi - a) ** m)
      else 0.0 * xi

    def yi(xi: A) = 1 + ((xi + 1.0) / 4.0)

    val xs = x.a :: x.b :: x.rest.toList
    val term1 = 10 * (sin(pi * yi(xs.head)) ** 2)
    val term2 = xs.sliding(2).toList.foldMap {
      case Seq(xi, xi1) =>
        val t1 = (yi(xi) - 1.0) ** 2
        val t2 = 1.0 + 10.0 * (sin(pi * yi(xi1)) ** 2)
        t1 * t2
    }
    val term3 = (yi(xs.last) - 1.0) ** 2
    val term4 = xs.foldMap(xi => u(xi, 10, 100, 4))

    (pi / 30.0) * (term1 + term2 + term3) + term4
  }

  def penalty2[F[_]: Foldable, A: Field : Order : Trig : Monoid](x: Sized2And[F, A]) = {
    def u(xi: A, a: Int, k: Int, m: Int) =
      if (xi > a) k * ((xi - a) ** m)
      else if (xi < -a) k * ((-xi - a) ** m)
      else 0.0 * xi

    val xs = (x.a :: x.b :: x.rest.toList)
    val term1 = sin(3.0 * pi * xs.head) ** 2
    val term2 = xs.sliding(2).toList.foldMap {
      case Seq(xi, xi1) =>
        val t1 = (xi - 1.0) ** 2
        val t2 = 1.0 + (sin(3.0 * pi * xi1) ** 2)
        t1 * t2
    }

    val term3 = ((xs.last - 1.0) ** 2) * (1.0 + sin(2.0 * pi * xs.last) ** 2)
    val term4 = xs.foldMap(xi => u(xi, 5, 100, 4))

    0.1 * (term1 + term2 + term3) + term4
  }

  def periodic[F[_]: Foldable1, A: Field : Trig : Monoid](x: F[A]) = {
    val t1 = x.foldMap(sin(_) ** 2)
    val t2 = 0.1 * exp(-x.foldMap(_ ** 2))
    1 + t1 - t2
  }

  def powellSum[F[_]: Foldable1, A: Ring : Signed : Monoid](x: F[A]) =
    x.toList.zipWithIndex.foldMap {
      case (xi, i) => abs(xi) ** (i + 1)
    }

  def price1[F[_]: Foldable1, A: Ring : Signed : Monoid](x: F[A]) =
    x.foldMap(xi => (abs(xi) - 5) ** 2)

  def price2[F[_]: Foldable1, A: Field : Trig : Monoid](x: F[A]) =
    1.0 + x.foldMap(xi => sin(xi) ** 2) - 0.1 * exp(-x.foldMap(_ ** 2))

  def qing[F[_]: Foldable1, A: Field : Monoid](x: F[A]) =
    x.toList.zipWithIndex.foldMap {
      case (xi, i) => ((xi ** 2) - (i + 1.0)) ** 2
    }

  def quadric[F[_]: Foldable1, A: Ring : Monoid](x: F[A]) =
    (1 to x.length).toList.foldMap { i =>
      (x.toList take i).foldMap(xi => xi) ** 2
    }

  def quintic[F[_]: Foldable1, A: Field : Signed : Monoid](x: F[A]) =
    abs(x.foldMap { xi =>
      (xi ** 5) - 3 * (xi ** 4) + 4 * (xi ** 3) + 2 * (xi ** 2) - 10.0 * xi - 4
    })

  def rastrigin[F[_]: Foldable1, A: Field : IsReal : Trig : Monoid](x: F[A]) =
    10 * x.length + x.foldMap(xi => xi ** 2 - 10 * cos(2 * pi * xi))

  def rosenbrock[F[_]: Foldable, A: Ring : Monoid](x: Sized2And[F, A]) =
    (x.a :: x.b :: x.rest.toList).sliding(2).toList.foldMap {
      case Seq(xi, xi1) => 100 * ((xi1 - (xi ** 2)) ** 2) + ((xi - 1) ** 2)
    }

  def rotatedEllipse1[F[_]: Foldable, A: Field : Monoid](x: Sized2And[F, A]) =
    (x.a :: x.b :: x.rest.toList).sliding(2).toList.foldMap {
      case Seq(x1, x2) =>
        (7 * (x1 ** 2)) - (6 * sqrt(3.0) * x1 * x2) + (13 * (x2 ** 2))
    }

  def rotatedEllipse2[F[_]: Foldable, A: Ring : Monoid](x: Sized2And[F, A]) =
    (x.a :: x.b :: x.rest.toList).sliding(2).toList.foldMap {
      case Seq(x1, x2) => (x1 ** 2) - (x1 * x2) + (x2 ** 2)
    }

  def salomon[F[_]: Foldable1,  A: Field : NRoot : Trig : Monoid](x: F[A]) = {
    val ss = sqrt(spherical(x))
    -cos(2 * pi * ss) + (0.1 * ss) + 1
  }

  def schaffer1[F[_]: Foldable, A: Field : Trig : Monoid](x: Sized2And[F, A]) =
    (x.a :: x.b :: x.rest.toList).sliding(2).toList.foldMap {
      case Seq(xi, xi1) =>
        val t1 = (xi ** 2) + (xi1 ** 2)
        val t2 = (xi ** 2) + (xi1 ** 2)
        val t3 = (sin((t1) ** 2) ** 2) - 0.5
        val t4 = (1 + 0.001 * t2) ** 2
        0.5 + (t3 / t4)
    }

  def schaffer2[F[_]: Foldable, A: Field : Trig : Monoid](x: Sized2And[F, A]) =
    (x.a :: x.b :: x.rest.toList).sliding(2).toList.foldMap {
      case Seq(xi, xi1) =>
        val t1 = (xi ** 2) - (xi1 ** 2)
        val t2 = (xi ** 2) + (xi1 ** 2)
        val t3 = (sin((t1) ** 2) ** 2) - 0.5
        val t4 = (1 + 0.001 * t2) ** 2
        0.5 + (t3 / t4)
    }

  def schaffer3[F[_]: Foldable, A: Field : Signed : Trig : Monoid](x: Sized2And[F, A]) =
    (x.a :: x.b :: x.rest.toList).sliding(2).toList.foldMap {
      case Seq(xi, xi1) =>
        val t1 = cos(abs((xi ** 2) - (xi1 ** 2)))
        val t2 = (xi ** 2) + (xi1 ** 2)
        val t3 = (sin((t1) ** 2) ** 2) - 0.5
        val t4 = (1 + 0.001 * t2) ** 2
        0.5 + (t3 / t4)
    }

  def schaffer4[F[_]: Foldable, A: Field : Trig : Monoid](x: Sized2And[F, A]) =
    (x.a :: x.b :: x.rest.toList).sliding(2).toList.foldMap {
      case Seq(xi, xi1) =>
        val t1 = sin((xi ** 2) - (xi1 ** 2))
        val t2 = (xi ** 2) + (xi1 ** 2)
        val t3 = (cos((t1) ** 2) ** 2) - 0.5
        val t4 = (1 + 0.001 * t2) ** 2
        0.5 + (t3 / t4)
    }

  def schwefel1[F[_]: Foldable1, A: Field : NRoot : Monoid](x: F[A]) =
    x.foldMap(_ ** 2) ** sqrt(pi)

  def schwefel12[F[_]: Foldable1, A: Ring : Monoid](x: F[A]) =
    x.toList.zipWithIndex.foldMap {
      case (xi, i) => x.toList.take(i + 1).foldMap(xi => xi) ** 2
    }

  def schwefel221[F[_]: Foldable, A: Order : Signed](x: Sized1And[F, A]) =
    x.foldLeft(abs(x.head)) { (xi, xi1) => spire.math.max(abs(xi), abs(xi1)) }

  def schwefel222[F[_]: Foldable1 : Functor, A: Signed : Monoid](x: F[A])(implicit A: Ring[A]) =
    x.foldMap(abs(_)) + x.map(abs(_)).foldLeft(A.one)(_ * _)

  def schwefel223[F[_]: Foldable1, A: Ring : Monoid](x: F[A]) =
    x.foldMap(_ ** 10)

  def schwefel225[F[_]: Foldable, A: Field : Monoid](x: Sized1And[F, A]) =
    x.foldMap(xi => ((xi - 1.0) ** 2) + ((x.head - (xi ** 2)) ** 2))

  def schwefel226[F[_]: Foldable1, A: Field : NRoot : Signed : Trig : Monoid](x: F[A]) =
    418.9829 * x.length - x.foldMap(xi => xi * sin(sqrt(abs(xi))))

  def schwefel24[F[_]: Foldable, A: Field : Monoid](x: Sized1And[F, A]) =
    x.foldMap(xi => ((x.head - 1) ** 2) + ((x.head - xi) ** 2))

  def spherical[F[_]: Foldable1, A: Ring : Monoid](x: F[A]) =
    x.foldMap(_ ** 2)

  def step1[F[_]: Foldable1, A: IsReal : Ring : Signed : Monoid](x: F[A]) =
    x.foldMap(xi => floor(abs(xi)))

  def step2[F[_]: Foldable1, A: Field : IsReal : Monoid](x: F[A]) =
    x.foldMap(xi => (floor(xi) + 0.5) ** 2)

  def step3[F[_]: Foldable1, A: IsReal : Ring : Monoid](x: F[A]) =
    x.foldMap(xi => floor(xi ** 2))

  def stretchedVSineWave[F[_]: Foldable, A: Field : NRoot : Trig : Monoid](x: Sized2And[F, A]) =
    (x.a :: x.b :: x.rest.toList).sliding(2).toList.foldMap {
      case Seq(xi, xi1) =>
        val t1 = ((xi1 ** 2) + (xi ** 2)) ** 0.25
        val t2 = sin(50.0 * (((xi1 ** 2) + (xi ** 2) ** 0.1))) ** 2 + 0.1
        t1 * t2
    }

  def styblinksiTang[F[_]: Foldable1, A: Field : Monoid](x: F[A]) =
    0.5 * x.foldMap(xi => (xi ** 4) - 16 * (xi ** 2) + 5 * xi)

  def sumSquares[F[_]: Foldable1, A: Ring : Monoid](x: F[A]) =
    x.toList.zipWithIndex.foldMap { case (xi, i) => (i + 1) * (xi ** 2) }

  def sumDifferentPowers[F[_]: Foldable1, A: Ring : Signed : Monoid](x: F[A]) =
    x.toList.zipWithIndex.foldMap { case (xi, i) => abs(xi) ** (i + 2) }

  def trigonometric1[F[_]: Foldable1, A: Field : Trig : Monoid](x: F[A]) =
    x.toList.zipWithIndex.foldMap {
      case (xi, i) =>
        (x.length - x.foldMap(cos(_)) + i * (1.0 - cos(xi) - sin(xi))) ** 2
    }

  def trigonometric2[F[_]: Foldable1, A: Field : Trig : Monoid](x: F[A]) =
    1.0 + x.foldMap { xi =>
      val co = (xi - 0.9) ** 2
      val t1 = 8.0 * (sin(7.0 * co) ** 2)
      val t2 = 6.0 * (sin(14.0 * co) ** 2)
      t1 + t2 + co
    }

  def vincent[F[_]: Foldable1, A: Field : Trig : Monoid](x: F[A]) =
    -x.foldMap(xi => sin(10.0 * log(xi)))

  def weierstrass[F[_]: Foldable1, A: Field : Trig : Monoid](x: F[A]) = {
    val a = 0.5
    val b = 3.0
    val kmax = 20
    val constant = (0 to kmax).toList.map { k =>
      val t1 = a ** k.toDouble
      val t2 = cos(2 * pi * (b ** k.toDouble) * 0.5)
      t1 * t2
    }.sum

    val factor1 = x.foldMap { xi =>
      (0 to kmax).toList.foldMap { k =>
        val t1 = a ** k.toDouble
        val t2 = cos(2 * pi * (b ** k.toDouble) * (xi + 0.5))
        t1 * t2
      }
    }

    factor1 - x.length * constant
  }

  def xinsheYang[F[_]: Foldable1, A: Field : Signed : Trig : Monoid](x: F[A]) = {
    val t1 = x.foldMap(abs(_))
    val t2 = exp(-x.foldMap(xi => sin(xi ** 2)))
    t1 * t2
  }

  def zakharov[F[_]: Foldable1, A: Field : IsReal : Monoid](x: F[A]) = {
    val t = x.toList.zipWithIndex.foldMap { case (xi, i) => 0.5 * i * xi }
    spherical(x) + (t ** 2) + (t ** 4)
  }

}
