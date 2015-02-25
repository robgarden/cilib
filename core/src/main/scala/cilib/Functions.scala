package cilib

import spire.math._
import spire.algebra._
import spire.implicits._

import breeze.linalg._

object Functions {

  def absoluteValue[T: Field : Signed](x: Seq[T]) = Some(x.map(abs(_)).qsum)

  def ackley[T: Field : IsReal : NRoot : Trig](x: Seq[T]) =
    if (!x.isEmpty) {
      val n = x.length
      val sumcos = x.map(xi => cos(2.0 * pi * xi)).qsum

      spherical(x).map(sum =>
          -20 * exp(-0.2 * sqrt(sum / n)) - exp(sumcos / n) + 20 + exp(1))
    } else None

  def adjiman[T: Field : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) => Some(cos(x1) * sin(x2) - (x1) / (x2 ** 2 + 1))
    case _           => None
  }

  def alpine1[T: Field : Signed : Trig](x: Seq[T]) =
    Some(x.map(xi => abs((xi * sin(xi)) + (0.1 * xi))).qsum)

//  def alpine2[T: Field : NRoot : Trig](x: Seq[T]) =
//    Some(x.map(xi => sqrt(xi) * sin(xi)).qproduct)

  def arithmeticMean[T: Field : NRoot](x: Seq[T]) =
    if (!x.isEmpty) {
      val n = x.length
      val avg = x.qsum / n
      val rootProd = x.qproduct ** (1.0 / n)
      Some((avg - rootProd) ** 2)
    } else None

  def bartelsConn[T: Field : Signed : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      val term1 = abs(x1 ** 2 + x2 ** 2 + x1 * x2)
      val term2 = abs(sin(x1))
      val term3 = abs(cos(x2))
      Some(term1 + term2 + term3)
    case _ => None
  }

  def beale[T: Field : IsReal](x: Seq[T]) = x match {
    case Seq(x1, x2) => 
      Some((1.5 - x1 + x1 * x2) ** 2 +
        (2.25 - x1 + x1 * (x2 ** 2)) ** 2 +
        (2.625 - x1 + x1 * (x2 ** 3)) ** 2)
    case _ => None
  }

  def bentCigar[T: Field](x: Seq[T]) =
    if (x.length >= 2)
      Some(x.head ** 2 + 10 ** 6 * x.tail.map(_ ** 2).qsum)
    else None

//  def biggsExp2[T: Field : Trig](x: Seq[T]) = x match {
//    case Seq(x1, x2) =>
//      Some((for { i <- 1 to 10 
//        ti = 0.1 * i
//        yi = exp(-ti) - 5.0 * exp(10.0 * ti)
//        t1 = exp(-ti * x1)
//        t2 = 5.0 * exp(-ti * x2)
//      } yield (t1 - t2 - yi) ** 2).qsum)
//    case _ => None
//  }

  def bird[T: Field : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) => 
      Some(sin(x1) * exp((1 - cos(x2)) ** 2) +
        cos(x2) * exp((1 - sin(x1)) ** 2) + (x1 - x2) ** 2)
    case _           => None
  }

  def bohachevsky1[T: Field : IsReal : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      Some((x1 ** 2) + 2 * (x2 ** 2) - 0.3 *
        cos(3 * pi * x1) - 0.4 * cos(4 * pi * x2) + 0.7)
    case _ => None
  }

  def bohachevsky2[T: Field : IsReal : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      Some((x1 ** 2) + 2 * (x2 ** 2) - 0.3 *
        cos(3 * pi * x1) * cos(4 * pi * x2) + 0.3)
    case _ => None
  }

  def bohachevsky3[T: Field : IsReal : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      Some((x1 ** 2) + 2 * (x2 ** 2) - 0.3 *
        cos(3 * pi * x1 + 4 * pi * x2) + 0.3)
    case _ => None
  }

  def booth[T: Field : IsReal](x: Seq[T]) = x match {
    case Seq(x1, x2) => Some((x1 + 2 * x2 - 7) ** 2 + (2 * x1 + x2 - 5) ** 2)
    case _           => None
  }

  def bukin4[T: Field : IsReal : Signed](x: Seq[T]) = x match {
    case Seq(x1, x2) => Some(100 * (x2 ** 2) + 0.01 * abs(x1 + 10))
    case _           => None
  }

  def bukin6[T: Field : IsReal : NRoot : Signed](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
          Some(100.0 * sqrt(abs(x2 - 0.01 * (x1 ** 2))) + 0.01 * abs(x1 + 10.0))
    case _ => None
  }

  def damavandi[T: Field : IsReal : Signed : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) => 
      val numer = sin(pi * (x1 - 2)) * sin(pi * (x2 - 2))
      val denum = pi ** 2 * (x1 - 2) * (x2 - 2)
      val factor1 = 1 - (abs(numer / denum) ** 5)
      val factor2 = 2 + (x1 - 7) ** 2 + 2 * (x2 - 7) ** 2
      Some(factor1 * factor2)
    case _ => None
  }

  def dejongF4[T: Field](x: Seq[T]) =
    Some(x.zipWithIndex.map { case (xi, i) => (i + 1) * (xi ** 4) }.qsum)

  def easom[T: Field : IsReal : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      Some(-cos(x1) * cos(x2) * exp(-((x1 - pi) ** 2 + (x2 - pi) ** 2)))
    case _ => None
  }

  def eggHolder[T: Field : IsReal : NRoot : Signed : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) => 
      Some(-(x2 + 47) * sin(sqrt(abs(x2 + (x1 / 2) + 47))) -
        x1 * sin(sqrt(abs(x1 - x2 - 47))))
    case _ => None
  }

  def elliptic[T: Field](x: Seq[T]) =
    if (x.length >= 2)
      Some(x.zipWithIndex.map { case (xi, i) =>
        (10e6 ** (i / (x.length - 1.0))) * (xi ** 2) }.qsum)
    else None

  def goldsteinPrice[T: Field : IsReal](x: Seq[T]) = x match {
    case Seq(x1, x2) => 
      val term1 = 1 + ((x1 + x2 + 1) ** 2) * (19 - 14 * x1 + 3 * (x1 ** 2) -
        14 * x2 + 6 * x1 * x2 + 3 * (x2 ** 2))
      val term2 = 30 + ((2 * x1 - 3 * x2) ** 2) * (18 - 32 * x1 + 12 *
        (x1 ** 2) + 48 * x2 - 36 * x1 * x2 + 27 * (x2 ** 2))
      Some(term1 * term2)
    case _ => None
  }

  def griewank[T: Field : NRoot : Trig](x: Seq[T]) = {
    val prod = x.zipWithIndex.map { case (xi, i) =>
      cos(xi / sqrt(i + 1))
    }.qproduct

    spherical(x).map(sum => 1 + sum * (1.0 / 4000.0) - prod)
  }

  def himmelblau[T: Field](x: Seq[T]) = x match {
    case Seq(x1, x2) => Some((x1 ** 2 + x2 - 11) ** 2 + (x1 + x2 ** 2 - 7) ** 2)
    case _           => None
  }

  def katsuura[T: Field : IsReal : NRoot](x: Seq[T]) = {
    val n = x.length

    val product = x.zipWithIndex.map { case (xi, i) =>
      val sum = (1 to 32).map { j =>
        val term = (2 ** j) * xi
        abs(term - round(term)) / (2 ** j)
      }.qsum
      (1 + i + sum) ** (10.0 / pow(n, 1.2))
    }.qproduct

    Some((10.0 / (n ** 2)) * product  - 1)
  }

  def levy[T: Field : IsReal : Trig](x: Seq[T]) =
    if (!x.isEmpty) {
      def w(i: Int) = 1 + (x(i) - 1) / 4

      val n = x.length
      val term1 = (sin(3 * pi * w(0))) ** 2

      val term2 = (0 until n - 1).map { i =>
        (w(i) - 1) ** 2 * (1 + sin(3 * pi * w(i) + 1) ** 2)
      }.qsum

      val term3 = (w(n - 1) - 1) ** 2 * (1 + sin(2 * pi * w(n - 1)) ** 2)

      Some(term1 + term2 + term3)
    } else None

  def matyas[T: Field : IsReal](x: Seq[T]) = x match {
    case Seq(x1, x2) => Some(0.26 * (x1 ** 2 + x2 ** 2) - 0.48 * x1 * x2)
    case _           => None
  }

  def maximum[T: Ordering](x: Seq[T]) = x match {
    case Seq() => None
    case _     => Some(x.max)
  }

  def minimum[T: Ordering](x: Seq[T]) = x match {
    case Seq() => None
    case _     => Some(x.min)
  }

  def michalewicz[T: Field : IsReal : NRoot : Trig](x: Seq[T]) = {
    val m = 10.0
    Some(-x.zipWithIndex.map { case (xi, i) =>
      sin(xi) * (sin(((i + 1) * (xi ** 2)) / pi) ** (2 * m))
    }.qsum)
  }

  def nastyBenchmark[T: Field](x: Seq[T]) =
    Some(x.zipWithIndex.map { case(xi, i) => (xi - (i + 1)) ** 2 })

  def rastrigin[T: Field : IsReal : Trig](x: Seq[T]) =
    Some(10 * x.size + x.map(xi => xi ** 2 - 10 * cos(2 * pi * xi)).qsum)

  def rosenbrock[T: Field](x: Seq[T]) =
    Some((0 until x.length - 1).map(i => {
      val term1 = 100 * (x(i + 1) - x(1) ** 2) ** 2
      val term2 = (x(1) - 1) ** 2
      term1 + term2
    }).qsum)

  def salomon[T: Field : NRoot : Trig](x: Seq[T]) =
    spherical(x).map(sum => -cos(2 * pi * sqrt(sum)) + (0.1 * sqrt(sum)) + 1)

  def schaffer2[T: Field : NRoot](x: Seq[T]) = x match {
    case Seq(_, _) => spherical(x).map(sum =>
      (sum ** 0.25) * (50 * (sum ** 0.1) + 1))
    case _         => None
  }

  def spherical[T: Field](x: Seq[T]) = Some(x.map(_ ** 2).qsum)

  def step[T: Field : IsReal](x: Seq[T]) =
    Some(x.map(xi => (floor(xi) + 0.5) ** 2).qsum)

  def schwefel[T: Field : IsReal : NRoot : Signed : Trig](x: Seq[T]) =
    Some(418.9829 * x.length - x.map(xi => xi * sin(sqrt(abs(xi)))).qsum)

  def schwefel12[T: Field](x: Seq[T]) =
    Some(x.zipWithIndex.map { case (xi, i) => x.take(i + 1).qsum ** 2 }.qsum)

  def schwefel221[T: Ordering : Signed](x: Seq[T]) = maximum(x.map(abs(_)))

  def schwefel222[T: Field : Signed](x: Seq[T]) =
    Some(x.map(abs(_)).qsum + x.map(abs(_)).qproduct)

  def shubert[T: Field : IsReal : Trig](x: Seq[T]) =
    if (x.length == 2)
      Some(x.map(xi => (1 to 5).map(j => j * cos((j + 1) * xi + j)).qsum).qproduct)
    else None

  def threeHumpCamelback[T: Field : IsReal](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      Some(2 * x1 ** 2 - 1.05 * x1 ** 4 + ((x1 ** 6) / 6) + x1 * x2 + x2 ** 2)
    case _ => None
  }

  def vincent[T: Field : IsReal : Trig](x: Seq[T]) = 
    Some(-x.map(xi => sin(10.0 * log(xi))).qsum)

  def weierstrass[T: Field : Trig](x: Seq[T]) = {
    val a = 0.5
    val b = 3.0
    val kmax = 20
    val constant = (for { k <- 0 to kmax
      t1 = a ** k
      t2 = cos(2 * pi * (b ** k) * 0.5)
    } yield t1 * t2).qsum

    val factor1 = x.map(xi => (for { k <- 0 to kmax
      t1 = a ** k
      t2 = cos(2 * pi * (b ** k) * (xi + 0.5))
    } yield t1 * t2).qsum).qsum

    factor1 - x.length * constant
  }

  def wood[T: Field](x: Seq[T]) =
    if (x.length == 4)
      x match {
        case Seq(x1, x2, x3, x4) =>
          Some(100 * (x1 ** 2 - x2) ** 2 + (x1 - 1) ** 2 + (x3 - 1) ** 2 +
            90 * (x3 ** 2 - x4) ** 2 + 10.1 * ((x2 - 1) ** 2 + (x4 - 1) ** 2) +
            19.8 * (x2 - 1) * (x4 - 1))
        case _ => None
      }
    else None

  def zakharov[T: Field : IsReal](x: Seq[T]) = {
    val term = x.zipWithIndex.map { case (xi, i) => 0.5 * i * xi }.qsum
    spherical(x).map(sum => sum + term ** 2 + term ** 4)
  }

  def zettle[T: Field](x: Seq[T]) =
    if (x.length == 2)
      x match {
        case Seq(x1, x2) => Some((x1 ** 2 + x2 ** 2 - 2 * x1) ** 2 + x1 / 4.0)
        case _           => None
      }
    else None

}

object FunctionWrappers {

  def shifted[T: Ring](f: (Seq[T]) => Option[T], horizontal: T, vertical: T) =
    (x: Seq[T]) => f(x.map(_ - horizontal)).map(_ - vertical)

  def rotated(f: (Seq[Double]) => Option[Double], dim: Int) = {
    // create rotation matrix using QR factorisation
    val rotation = qr.justQ(DenseMatrix.rand(dim, dim))

    (x: Seq[Double]) => f((rotation * DenseVector(x.toArray)).toArray)
  }

}
