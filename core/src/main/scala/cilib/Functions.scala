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

  def boxBettsQuadraticSum[T: Field : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2, x3) =>
      val k = 10
      val values = for { i <- 1 to k
        i1 = i + 1
        co = -0.1 * (i1)
        t1 = exp(co * x1)
        t2 = exp(co * x2)
        t3 = exp((co - exp(-(i1))) * x3)
      } yield t1 - t2 - t3

      Some(values.map(_ ** 2).qsum)
    case _ => None
  }

  def brad[T: Field](x: Seq[T]) = x match {
    case Seq(x1, x2, x3) =>
      val y = List(
        0.14, 0.18, 0.22, 0.25, 0.29,
        0.32, 0.35, 0.39, 0.37, 0.58,
        0.73, 0.96, 1.34, 2.10, 4.39
      )
      val values = for { i <- 1 to 15
        ui = i
        vi = 16 - i
        wi = spire.math.min(ui, vi)
      } yield (y(i - 1) - x1 - ui) / (vi * x2 + wi * x3)
      Some(values.map(_ ** 2).qsum)
    case _ => None
  }

  def braninRCOS1[T: Field : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      val term1 = (x2 - (5.1 / (4.0 * (pi ** 2))) * (x1 ** 2) +
        (5.0 / pi) * x1 - 6.0) ** 2
      val term2 = 10.0 * (1.0 - 1 / (8.0 * pi)) * cos(x1)
      Some(term1 + term2 + 10.0)
    case _ => None
  }

  def brent[T: Field : Trig](x: Seq[T]) =
    Some(x.map(xi => (xi + 10) ** 2).qsum + exp(-x.map(_ ** 2).qsum))

  def brown[T: Field](x: Seq[T])(implicit nr: NRoot[T]) =
    if (x.length >= 2) {
      Some(x.sliding(2).toList.map { s => s match {
        case Seq(xi, xi1) =>
          nr.fpow(xi ** 2, (xi1 ** 2) + 1) + nr.fpow(xi1 ** 2, (xi ** 2) + 1)
      }}.qsum)
    } else None

  def bukin2[T: Field](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      Some(100 * (x2 - 0.01 * (x1 ** 2) + 1) ** 2 + 0.01 * ((x1 + 10) ** 2))
    case _ => None
  }

  def bukin2Adapted[T: Field](x: Seq[T]) = x match {
    case Seq(_, _) => bukin2(x).map(_ ** 2)
    case _ => None
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

  def centralTwoPeakTrap[T: Field : Order](x: Seq[T]) = x match {
    case Seq(x1) =>
      if      (x1 < 0)   Some(0.0 * x1)
      else if (x1 <= 10) Some((-160.0 / 10.0) * x1)
      else if (x1 <= 15) Some((-160.0 / 5.0) * (15.0 - x1))
      else if (x1 <= 20) Some((-200.0 / 5.0) * (x1 - 15.0))
      else               Some((0.0 * x1) - 200.0)
    case _ => None
  }

  def chichinadze[T: Field : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      val term1 = (x1 ** 2) - (12.0 * x1) + 11.0
      val term2 = 10.0 * cos(pi * (x1 / 2.0)) + 8.0 * sin(5.0 * pi * x1)
      val term3 = ((1.0 / 5.0) ** 0.5) * exp(-0.5 * ((x2 - 0.5) ** 2))
      Some(term1 + term2 - term3)
    case _ => None
  }

  def chungReynolds[T: Field](x: Seq[T]) = spherical(x).map(_ ** 2)

  def cigar[T: Field](x: Seq[T]) = x match {
    case Seq() => None
    case x1 :: xs => spherical(xs).map(s => (x1 ** 2) + (10 ** 6) * s)
  }

  def colville[T: Field](x: Seq[T]) = x match {
    case Seq(x1, x2, x3, x4) =>
      val term1 = 100.0 * ((x1 - (x2 ** 2)) ** 2) + ((1.0 - x1) ** 2)
      val term2 = 90.0 * ((x4 - x3) ** 2) + ((1.0 - x3) ** 2)
      val term3 = 10.1 * (((x2 - 1.0) ** 2) + ((x4 - 1.0) ** 2))
      val term4 = 19.8 * (x2 - 1.0) * (x4 - 1.0)
      Some(term1 + term2 + term3 + term4)
    case _ => None
  }

  def cosineMixture[T: Field : Trig](x: Seq[T]) =
    spherical(x).map(sum => - 0.1 * x.map(xi => cos(5 * pi * xi)).qsum + sum)

  def crossInTray[T: Field : NRoot : Signed : Trig](x: Seq[T]) = x match {
    case Seq() => None
    case _ =>
      val term1 = x.map(sin(_)).qproduct
      val term2 = spherical(x).map(sum => exp(abs(100.0 - (sqrt(sum) / pi))))
      term2.map(t2 => -0.0001 * ((abs(term1 * t2) + 1.0) ** 0.1))
  }

  def crossLegTable[T: Field : NRoot : Signed : Trig](x: Seq[T]) =
    crossInTray(x).map(denom => -1.0 / (denom / -0.0001))

  def crossCrowned[T: Field : NRoot : Signed : Trig](x: Seq[T]) =
    crossInTray(x).map(-_)

  def csendes[T: Field : Trig](x: Seq[T]) =
    Some(x.map(xi => (xi ** 6) * (2 + sin(1.0 / xi))).qsum)

  def cube[T: Field](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      Some(100.0 * ((x2 - (x1 ** 3)) ** 2) + ((1.0 - x1) ** 2))
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

  def deb1[T: Field : Trig](x: Seq[T]) = x match {
    case Seq() => None
    case _ => Some(-(1.0 / x.length) * x.map(xi => sin(5 * pi * xi) ** 6).qsum)
  }

  def deb3[T: Field : NRoot : Trig](x: Seq[T]) =
    deb1(x.map(xi => (xi ** 0.75) - 0.05))

  def decanomial[T: Field : Signed](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      val coX1 = List(1, -20, 180, -960, 3360, -8064,
                        13340, -15360, 11520, -5120, 2624)
      val coX2 = List(1, 12, 54, 108, 81)

      val term1 = abs(coX2.zipWithIndex.map {
        case (ci, i) => ci * (x2 ** (coX2.length - 1 - i))
      }.qsum)

      val term2 = abs(coX1.zipWithIndex.map {
        case (ci, i) => ci * (x1 ** (coX1.length - 1 - i))
      }.qsum)

      Some(0.001 * ((term1 + term2) ** 2))
    case _ => None
  }

  def deckkersAarts[T: Field](x: Seq[T]) = x.map(_ ** 2) match {
    case Seq(x1, x2) =>
      val term1 = (10 ** 5) * x1 + x2
      val term2 = (x1 + x2) ** 2
      val term3 = (1.0 / (10 ** 5)) * ((x1 + x2) ** 4)
      Some(term1 - term2 + term3)
    case _ => None
  }

  def deVilliersGlasser1[T: Field : NRoot : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2, x3, x4) =>
      Some((for { i <- 1 to 24
        ti = 0.1 * (i - 1)
        yi = 60.137 * (1.371 ** ti) * sin(3.112 * ti + 1.761)
        term1 = x1 * (x2 ** ti)
        term2 = sin(x3 * ti + x4)
        term3 = yi
      } yield (term1 * term2 - term3) ** 2).qsum)
    case _ => None
  }

  def deVilliersGlasser2[T: Field : NRoot : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2, x3, x4, x5) =>
      Some((for { i <- 1 to 16
        ti = 0.1 * (i - 1)
        yi = 53.81 * (1.27 ** ti) * tanh(3.012 * ti + sin(2.13 * ti)) *
          cos(exp(0.507) * ti)
        term1 = x1 * (x2 ** ti)
        term2 = tanh(x3 * ti + sin(x4 * ti))
        term3 = cos(ti * exp(x5))
        term4 = yi
      } yield (term1 * term2 * term3 - term4) ** 2).qsum)
    case _ => None
  }

  def differentPowers[T: Field : NRoot : Signed](x: Seq[T]) =
    if (x.length >= 2) {
      val mapped = x.zipWithIndex.map {
        case (xi, i) => abs(xi) ** (2 + ((4.0 * i) / (x.length - 1.0)))
      }
      Some(sqrt(mapped.qsum))
    } else None

  def discus[T: Field](x : Seq[T]) = x match {
    case Seq() => None
    case x1 :: xs => spherical(xs).map(s => (10 ** 6) * (x1 ** 2) + s)
  }

  def dixonPrice[T: Field](x: Seq[T]) = x match {
    case Seq() => None
    case Seq(_) => None
    case x1 :: xs =>
      def term(l: (Seq[T], Int)) = l match {
        case (Seq(xi, xi1), i) => (i + 2) * (((2 * (xi1 ** 2)) - xi) ** 2)
      }

      val term1 = ((x1 - 1) ** 2)
      val term2 = x.sliding(2).toList.zipWithIndex.map(term).qsum
      Some(term1 + term2)
  }

  def dropWave[T: Field : NRoot : Trig](x: Seq[T]) =
    spherical(x).map(s => -(1 + cos(12 * sqrt(s))) / (2 + 0.5 * s))

  def easom[T: Field : IsReal : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      Some(-cos(x1) * cos(x2) * exp(-((x1 - pi) ** 2 + (x2 - pi) ** 2)))
    case _ => None
  }

  def eggCrate[T: Field : Trig](x: Seq[T]) = spherical(x).map { s =>
    spherical(x.map(sin(_))).map(s1 => s + 24 * s1)
  }.flatten

  def eggHolder[T: Field : NRoot : Signed : Trig](x: Seq[T]) =
    if (x.length >= 2) {
      def g(l: Seq[T]) = l match {
        case Seq(x1, x2) =>
          -(x2 + 47) * sin(sqrt(abs(x2 + (x1 / 2) + 47))) -
            x1 * sin(sqrt(abs(x1 - x2 - 47)))
      }
      Some(x.sliding(2).toList.map(g).qsum)
    } else None

  def elliptic[T: Field](x: Seq[T]) =
    if (x.length >= 2)
      Some(x.zipWithIndex.map { case (xi, i) =>
        (10e6 ** (i / (x.length - 1.0))) * (xi ** 2) }.qsum)
    else None

  def elAttarVidyasagarDutta[T: Field](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      val term1 = ((x1 ** 2) + x2 - 10) ** 2
      val term2 = (x1 + (x2 ** 2) - 7) ** 2
      val term3 = ((x1 ** 2) + (x2 ** 3) - 1) ** 2
      Some(term1 + term2 + term3)
    case _ => None
  }

  def exponential1[T: Field : Trig](x: Seq[T]) =
    spherical(x).map(s => -exp(-0.5 * s))

  def exponential2[T: Field : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      Some((for { i <- 0 to 9
        term1 = 1.0 * exp(-i * x1 / 10.0)
        term2 = 5.0 * exp(-i * x2 / 10.0)
        term3 = 1.0 * exp(-i / 10.0)
        term4 = 5.0 * exp(-i)
      } yield (term1 - term2 - term3 + term4) ** 2).qsum)
    case _ => None
  }

  def freudensteinRoth[T: Field](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      val term1 = (x1 - 13 + ((5 - x2) * x2 - 2) * x2) ** 2
      val term2 = (x1 - 29 + ((x2 + 1) * x2 -14) * x2) ** 2
      Some(term1 + term2)
    case _ => None
  }

  def gear[T: Field : IsReal](x: Seq[T]) = x.map(floor(_)) match {
    case Seq(x1, x2, x3, x4) =>
      val term1 = 1.0 / 6.931
      val numer = x1 * x2
      val denom = x3 * x4
      Some((term1 - (numer / denom)) ** 2)
    case _ => None
  }

  def giunta[T: Field : Trig](x: Seq[T]) = x match {
    case Seq(_, _) =>
      val mapped = x.map { xi =>
        val factor = (16.0 / 15.0) * xi - 1
        val term1 = sin(factor)
        val term2 = term1 ** 2
        val term3 = (1.0 / 50.0) * sin(4 * factor)
        term1 + term2 + term3
      }
      Some(0.6 + mapped.qsum)
    case _ => None
  }

  def goldsteinPrice1[T: Field](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      val term1 = 1 + ((x1 + x2 + 1) ** 2) * (19 - 14 * x1 + 3 * (x1 ** 2) -
        14 * x2 + 6 * x1 * x2 + 3 * (x2 ** 2))
      val term2 = 30 + ((2 * x1 - 3 * x2) ** 2) * (18 - 32 * x1 + 12 *
        (x1 ** 2) + 48 * x2 - 36 * x1 * x2 + 27 * (x2 ** 2))
      Some(term1 * term2)
    case _ => None
  }

  def goldsteinPrice2[T: Field : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      val term1 = exp(0.5 * (((x1 ** 2) + (x2 ** 2) - 25) ** 2))
      val term2 = sin(4 * x1 - 3 * x2) ** 4
      val term3 = 0.5 * ((2 * x1 + x2 - 10) ** 2)
      Some(term1 + term2 + term3)
    case _ => None
  }

  def griewank[T: Field : NRoot : Trig](x: Seq[T]) = {
    val prod = x.zipWithIndex.map { case (xi, i) =>
      cos(xi / sqrt(i + 1))
    }.qproduct

    spherical(x).map(s => 1 + s * (1.0 / 4000.0) - prod)
  }

  def hansen[T: Field : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      val term1 = (for (i <- 0 to 4)
        yield (i + 1) * cos(i * x1 + i + 1)).qsum
      val term2 = (for (j <- 0 to 4)
        yield (j + 1) * cos((j + 2) * x2 + j + 1)).qsum
      Some(term1 * term2)
    case _ => None
  }

  def hartman3[T: Field : Trig](x: Seq[T]) =
    if (x.length == 3) {

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

      val terms = for { i <- 0 until 4
        power = for (j <- 0 until 3) yield a(i)(j) * ((x(j) - p(i)(j)) ** 2)
      } yield c(i) * exp(-power.qsum)

      Some(-terms.qsum)
    } else None

  def hartman6[T: Field : Trig](x: Seq[T]) =
    if (x.length == 6) {

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

      val terms = for { i <- 0 until 4
        power = for (j <- 0 until 6) yield a(i)(j) * ((x(j) - p(i)(j)) ** 2)
      } yield c(i) * exp(-power.qsum)

      Some(-terms.qsum)

    } else None

  def helicalValley[T: Field : NRoot : Order : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2, x3) =>

      val theta = {
        val term  = if (x1 >= (x1 * 0)) 0.0 else 0.5
        atan((x2 / x1) + term) / (2 * pi)
      }

      val term1 = (x2 - 10.0 * theta) ** 2
      val term2 = sqrt((x1 ** 2) + (x2 ** 2)) - 1.0
      val term3 = x3 ** 2

      Some(100.0 * (term1 + term2) + term3)

    case _ => None
  }

  def himmelblau[T: Field](x: Seq[T]) = x match {
    case Seq(x1, x2) => Some((x1 ** 2 + x2 - 11) ** 2 + (x1 + x2 ** 2 - 7) ** 2)
    case _           => None
  }

  def holzman[T: Field : Trig](x: Seq[T])(implicit nr: NRoot[T]) = x match {
    case Seq(x1, x2, x3) =>
      val terms = for { i <- 0 to 99
        ui    = 25.0 + ((-50.0 * log(0.01 * (i + 1))) ** (2.0 / 3.0))
        term1 = exp(-nr.fpow(ui - x2, x3) / x1)
        term2 = 0.01 * (i + 1)
      } yield (term1 - term2)
      Some(terms.qsum)
    case _ => None
  }

  def hosaki[T: Field : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      val term1 = 1 - 8 * x1 + 7 * (x1 ** 2)
      val term2 = (7.0 / 3.0) * (x1 ** 3)
      val term3 = (1.0 / 4.0) * (x1 ** 4)
      val term4 = (x2 ** 2) * exp(-x2)
      Some((term1 - term2 + term3) * term4)
    case _ => None
  }

  def hyperEllipsoid[T: Field](x: Seq[T]) =
    Some(x.zipWithIndex.map { case (xi, i) => i * (xi ** 2) }.qsum)

  def hyperEllipsoidRotated[T: Field](x: Seq[T]) = {
    val values = for (i <- 1 to x.length) yield x take i
    Some(values.map(spherical(_)).flatten.qsum)
  }

  def infinity[T: Field : Trig](x: Seq[T]) =
    if (x.forall(_ != 0.0))
      Some(x.map(xi => (xi ** 6) * (sin(1.0 / xi) + 2)).qsum)
    else None

  def jennrichSampson[T: Field : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      val terms = for { i <- 1 to 10
        term1 = 2 + 2 * i
        term2 = exp(i * x1) + exp(i * x2)
      } yield (term1 - term2) ** 2
      Some(terms.qsum)
    case _ => None
  }

  def judge[T: Field](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
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

      val term1 = mappedB.zip(mappedC).map { case (ai, bi) => ai + bi }
      val term2 = term1.map(_ + x1)
      val term3 = term2.zip(A).map { case (t2, ai) => t2 - ai }
      Some(term3.map(_ ** 2).qsum)

    case _ => None
  }

  def katsuura[T: Field : IsReal : NRoot](x: Seq[T]) = {
    val n = x.length

    val terms = x.zipWithIndex.map { case (xi, i) =>
      val term1 = i + 1
      val d = 32
      val term2 = for (k <- 1 to d) yield floor((2 ** k) * xi) * (1.0 / (2 ** k))
      1 + term1 * term2.qsum
    }

    Some(terms.qproduct)
  }

  def kowalik[T: Field](x: Seq[T]) = x match {
    case Seq(x1, x2, x3, x4) =>
      val b = List(
        4.0, 2.0, 1.0, 0.5, 0.25, 1.0 / 6.0, 0.125,
        0.1, 1.0 / 12.0, 1.0 / 14.0, 0.0625
      )

      val a = List(
        0.1957, 0.1947, 0.1735, 0.1600, 0.0844, 0.0627,
        0.0456, 0.0342, 0.0323, 0.0235, 0.0246
      )

      val terms = a.zip(b).map { case (ai, bi) =>
        val numer = x1 * ((bi ** 2) + bi * x2)
        val denom = (bi ** 2) + bi * x3 + x4
        ai - (numer / denom)
      }
      Some(terms.map(_ ** 2).qsum)
    case _ => None
  }

  def leon[T: Field](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      val term1 = 100 * ((x2 - (x1 ** 2)) ** 2)
      val term2 = (1 - x1) ** 2
      Some(term1 + term2)
    case _ => None
  }

  def levy3[T: Field : Trig](x: Seq[T]) =
    if (x.length >= 2) {
      def y(xi: T): T = 1 + (xi - 1) / 4.0

      val term1 = sin(pi * y(x.head)) ** 2

      val term2 = x.sliding(2).toList.map {
        case List(xi, xi1) =>
          ((y(xi) - 1) ** 2) * (1 + 10 * ((pi * y(xi1) ** 2)))
      }.qsum

      val term3 = (y(x.last) - 1) ** 2

      Some(term1 + term2 + term3)
    } else None

  def levy5[T: Field : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      val term1 = (for (i <- 1 to 5) yield i * cos((i - 1) * x1 + i)).qsum
      val term2 = (for (j <- 1 to 5) yield j * cos((j + 1) * x2 + j)).qsum
      val term3 = (x1 + 1.42513) ** 2
      val term4 = (x2 + 0.80032) ** 2
      Some(term1 * term2 + term3 + term4)
    case _ => None
  }

  def levy13[T: Field : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      val term1 = ((x1 - 1) ** 2) * ((sin(3 * pi * x2) ** 2) + 1)
      val term2 = ((x2 - 1) ** 2) * ((sin(2 * pi * x2) ** 2) + 1)
      val term3 = sin(3 * pi * x1) ** 2
      Some(term1 + term2 + term3)
    case _ => None
  }

  def levyMontalvo2[T: Field : Trig](x: Seq[T]) =
    if (x.length >= 2) {
      val term1 = sin(3 * pi * x.head) ** 2

      def term(a: Seq[T]) = a match {
        case Seq(xi, xi1) =>
          ((xi - 1) ** 2) * ((sin(3 * pi * xi1) ** 2) + 1 )
      }

      val term2 = x.sliding(2).toList.map(term).qsum

      val term3 = ((x.last - 1) ** 2) * ((sin(2 * pi * x.last) ** 2) + 1)

      Some(0.1 * (term1 + term2 + term3))
    } else None

  def matyas[T: Field : IsReal](x: Seq[T]) = x match {
    case Seq(x1, x2) => Some(0.26 * (x1 ** 2 + x2 ** 2) - 0.48 * x1 * x2)
    case _           => None
  }

  def maximum[T: Ordering](x: Seq[T]) = x match {
    case Seq() => None
    case _     => Some(x.max)
  }

  def mcCormick[T: Field : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      val term1 = sin(x1 + x2) + ((x1 - x2) ** 2)
      val term2 = -1.5 * x1 + 2.5 * x2 + 1
      Some(term1 + term2)
    case _ => None
  }

  def michalewicz[T: Field : IsReal : NRoot : Trig](x: Seq[T]) = {
    val m = 10.0
    Some(-x.zipWithIndex.map { case (xi, i) =>
      sin(xi) * (sin(((i + 1) * (xi ** 2)) / pi) ** (2 * m))
    }.qsum)
  }

  def mieleCantrell[T: Field : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2, x3, x4) =>
      val term1 = (exp(-x1) - x2) ** 4
      val term2 = 100 * ((x2 - x3) ** 6)
      val term3 = tan(x3 - x4) ** 4
      val term4 = x1 ** 8
      Some(term1 + term2 + term3 + term4)
    case _ => None
  }

  def minimum[T: Ordering](x: Seq[T]) = x match {
    case Seq() => None
    case _     => Some(x.min)
  }

  def mishra1[T: Field](x: Seq[T])(implicit nr: NRoot[T]) =
    if (!x.isEmpty) {
      val sum = x.init.qsum
      val n = x.length
      Some(nr.fpow(1 + n - sum, n - sum))
    } else None

  def mishra2[T: Field](x: Seq[T])(implicit nr: NRoot[T]) =
    if (x.length >= 2) {

      def term(a: Seq[T]) = a match {
        case Seq(xi, xi1) => 0.5 * (xi + xi1)
      }

      val sum = x.sliding(2).toList.map(term).qsum
      val n = x.length

      Some(nr.fpow(1 + n - sum, n - sum))
    } else None

  def mishra3[T: Field : NRoot : Signed : Trig](x: Seq[T]) = {
    val sum = spherical(x)
    sum.map(s => sqrt(abs(cos(sqrt(s)))) + 0.01 * x.qsum)
  }

  def mishra5[T: Field : Trig](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      val term1 = sin((cos(x1) + cos(x2)) ** 2) ** 2
      val term2 = cos((sin(x1) + sin(x2)) ** 2) ** 2
      val term3 = 0.01 * (x1 + x2)
      Some(((term1 + term2 + x1) ** 2) + term3)
    case _ => None
  }

  def mishra8[T: Field : Signed](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      val coX1 = List(1, -20, 180, -960, 3360, -8064,
                      13340, -15360, 11520, -5120, 2624)
      val coX2 = List(1, 12, 54, 108, 81)

      val term1 = abs(coX1.zipWithIndex.map {
        case (ci, i) => ci * (x1 ** (coX1.length - 1 - i))
      }.qsum)

      val term2 = abs(coX2.zipWithIndex.map {
        case (ci, i) => ci * (x2 ** (coX2.length - 1 - i))
      }.qsum)

      Some(0.001 * ((term1 * term2) ** 2))
    case _ => None
  }

  def mishra11[T: Field : NRoot : Signed](x: Seq[T]) = {
    val n = x.length
    val term1 = (1.0 / n) * x.map(abs(_)).qsum
    val term2 = x.map(abs(_)).qproduct ** (1.0 / n)
    Some((term1 - term2) ** 2)
  }

  def multiModal[T: Field : Signed](x: Seq[T]) =
    Some(x.map(abs(_)).qproduct * x.map(abs(_)).qsum)


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

  def rotatedEllipse1[T: Field](x: Seq[T]) =
    if (x.length >= 2) {
      def g(l: Seq[T]) = l match {
        case Seq(x1, x2) =>
          (7 * (x1 ** 2)) - (6 * sqrt(3.0) * x1 * x2) + (13 * (x2 ** 2))
      }
      Some(x.sliding(2).toList.map(g(_)).qsum)
    } else None

  def rotatedEllipse2[T: Field](x: Seq[T]) =
    if (x.length >= 2) {
      def g(l: Seq[T]) = l match {
        case Seq(x1, x2) => (x1 ** 2) - (x1 * x2) + (x2 ** 2)
      }
      Some(x.sliding(2).toList.map(g(_)).qsum)
    } else None

  def salomon[T: Field : NRoot : Trig](x: Seq[T]) =
    spherical(x).map(sum => -cos(2 * pi * sqrt(sum)) + (0.1 * sqrt(sum)) + 1)

  def schaffer2[T: Field : NRoot](x: Seq[T]) = x match {
    case Seq(_, _) => spherical(x).map(sum =>
      (sum ** 0.25) * (50 * (sum ** 0.1) + 1))
    case _         => None
  }

  def sixHumpCamelback[T: Field](x: Seq[T]) = x match {
    case Seq(x1, x2) =>
      val termX1 = 4 * (x1 ** 2) - 2.1 * (x1 ** 4) + (1.0 / 3.0) * (x1 ** 6)
      val termX2 = x1 * x2 - 4 * (x2 ** 2) + 4 * (x2 ** 4)
      Some(termX1 + termX2)
    case _ => None
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

  def wood[T: Field](x: Seq[T]) = x match {
    case Seq(x1, x2, x3, x4) =>
      val term1 = 100 * (((x1 ** 2) - x2) ** 2)
      val term2 = ((x1 - 1) ** 2) + ((x3 - 1) ** 2)
      val term3 = 90 * ((x3 ** 2 - x4) ** 2)
      val term4 = 10.1 * ((x2 - 1) ** 2)
      val term5 = ((x4 - 1) ** 2) + 19.8 * (x2 - 1) * (x4 - 1)
      Some(term1 + term2 + term3 + term4 + term4)
    case _ => None
  }

  def zakharov[T: Field : IsReal](x: Seq[T]) = {
    val term = x.zipWithIndex.map { case (xi, i) => 0.5 * i * xi }.qsum
    spherical(x).map(sum => sum + term ** 2 + term ** 4)
  }

  def zettle[T: Field](x: Seq[T]) = x match {
    case Seq(x1, x2) => Some((x1 ** 2 + x2 ** 2 - 2 * x1) ** 2 + x1 / 4.0)
    case _           => None
  }

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
