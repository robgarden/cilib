package cilib

import cilib.Interval._

import spire.math._
import spire.implicits._
import spire.math.Numeric._

object Functions {

  def absoluteValue(x: List[Double]) = Some(x.map(abs).sum)

  def ackley(x: List[Double]) = {
    val domain = Interval(Closed(-32.768), Closed(32.768))
    if (x in domain) {
      val n = x.length
      val sumcos = x.map(xi => cos(2.0 * pi * xi)).sum

      spherical(x).map(sum =>
          -20 * exp(-0.2 * sqrt(sum / n)) - exp(sumcos / n) + 20 + exp(1))
    } else None
  }

  def alpine(x: List[Double]) =
    Some(x.map(xi => abs((xi * sin(xi)) + (0.1 * xi))).sum)

  def beale(x: List[Double]) = {
    val domain = Interval(Closed(-4.5), Closed(4.5))
    x match {
      case List(x1, x2) =>
        if (x in domain)
          Some((1.5 - x1 + x1 * x2) ** 2 +
               (2.25 - x1 + x1 * (x2 ** 2)) ** 2 +
               (2.625 - x1 + x1 * (x2 ** 3)) ** 2)
        else None
      case _ => None
    }
  }

  def bentCigar(x: List[Double]) =
    if (x.length >= 2)
      Some(x.head ** 2 + 10 ** 6 * x.tail.map(_ ** 2).sum)
    else None

  def bird(x: List[Double]) = x match {
    case List(x1, x2) => Some(sin(x1) * exp((1 - cos(x2)) ** 2) +
                              cos(x2) * exp((1 - sin(x1)) ** 2) +
                              (x1 - x2) ** 2)
    case _            => None
  }

  val domain100 = Interval(Closed(-100.0), Closed(100.0))

  def bohachevsky1(x: List[Double]) = {
    x match {
      case List(x1, x2) =>
        if (x in domain100)
          Some((x1 ** 2) + 2 * (x2 ** 2) - 0.3 *
              cos(3 * pi * x1) - 0.4 * cos(4 * pi * x2) + 0.7)
        else None
      case _ => None
    }
  }

  def bohachevsky2(x: List[Double]) = {
    x match {
      case List(x1, x2) =>
        if (x in domain100)
          Some((x1 ** 2) + 2 * (x2 ** 2) - 0.3 *
              cos(3 * pi * x1) * cos(4 * pi * x2) + 0.3)
        else None
      case _ => None
    }
  }

  def bohachevsky3(x: List[Double]) = {
    x match {
      case List(x1, x2) =>
        if (x in domain100)
          Some((x1 ** 2) + 2 * (x2 ** 2) - 0.3 *
              cos(3 * pi * x1 + 4 * pi * x2) + 0.3)
        else None
      case _ => None
    }
  }

  def booth(x: List[Double]) = {
    val domain = Interval(Closed(-10.0), Closed(10.0))
    x match {
      case List(x1, x2) =>
        if (x in domain)
          Some((x1 + 2 * x2 - 7) ** 2 + (2 * x1 + x2 - 5) ** 2)
        else None
      case _ => None
    }
  }

  def bukin4(x: List[Double]) = {
    val domain1 = Interval(Closed(-15.0), Closed(-5.0))
    val domain2 = Interval(Closed(-3.0),  Closed(3.0))
    x match {
      case List(x1, x2) =>
        if ((x1 in domain1) && (x2 in domain2))
          Some(100 * (x2 ** 2) + 0.01 * abs(x1 + 10))
        else None
      case _ => None
    }
  }

  def bukin6(x: List[Double]) = {
    val domain1 = Interval(Closed(-15.0), Closed(-5.0))
    val domain2 = Interval(Closed(-3.0),  Closed(3.0))
    x match {
      case List(x1, x2) =>
        if ((x1 in domain1) && (x2 in domain2))
          Some(100.0 * sqrt(abs(x2 - 0.01 * (x1 ** 2))) + 0.01 * abs(x1 + 10.0))
        else None
    case _ => None
    }
  }

  def damavandi(x: List[Double]) = {
    val domain = Interval(Closed(0.0), Closed(14.0))
    x match {
      case List(x1, x2) => if (x in domain) {
        val numer = sin(pi * (x1 - 2)) * sin(pi * (x2 - 2))
        val denum = pi ** 2 * (x1 - 2) * (x2 - 2)
        val factor1 = 1 - (abs(numer / denum) ** 5)
        val factor2 = 2 + (x1 - 7) ** 2 + 2 * (x2 - 7) ** 2
        Some(factor1 * factor2)
      } else None
      case _ => None
    }
  }

  def dejongF4(x: List[Double]) =
    Some(x.zipWithIndex.map { case (xi, i) => (i + 1) * (xi ** 4) }.sum)

  def easom(x: List[Double]) = {
    val domain = Interval(Closed(-100.0), Closed(100.0))
    x match {
      case List(x1, x2) =>
        if (x in domain)
          Some(-cos(x1) * cos(x2) * exp(-((x1 - pi) ** 2 + (x2 - pi) ** 2)))
        else None
      case _ => None
    }
  }

  def eggHolder(x: List[Double]) = {
    val domain = Interval(Closed(-512.0), Closed(512.0))
    x match {
      case List(x1, x2) => if (x in domain)
        Some(-(x2 + 47) * sin(sqrt(abs(x2 + (x1 / 2) + 47))) -
             x1 * sin(sqrt(abs(x1 - x2 - 47))))
        else None
      case _ => None
    }
  }

  def goldsteinPrice(x: List[Double]) = {
    val domain = Interval(Closed(-2.0), Closed(2.0))
    x match {
      case List(x1, x2) => if (x in domain) {
          val term1 = 1 + ((x1 + x2 + 1) ** 2) * (19 - 14 * x1 + 3 * (x1 ** 2) -
                      14 * x2 + 6 * x1 * x2 + 3 * (x2 ** 2))
          val term2 = 30 + ((2 * x1 - 3 * x2) ** 2) * (18 - 32 * x1 + 12 *
                      (x1 ** 2) + 48 * x2 - 36 * x1 * x2 + 27 * (x2 ** 2))
          Some(term1 * term2)
      } else None
      case _ => None
    }
  }

  def griewank(x: List[Double]) = {
    val prod = x.zipWithIndex.map { case (xi, i) =>
      cos(xi / sqrt(i + 1))
    }.product

    spherical(x).map(sum => 1 + sum * (1.0 / 4000.0) - prod)
  }

  def himmelblau(x: List[Double]) = x match {
    case List(x1, x2) => Some((x1 ** 2 + x2 - 11) ** 2 + (x1 + x2 ** 2 - 7) ** 2)
    case _            => None
  }

  def katsuura(x: List[Double]) = {
    val domain = Interval(Closed(-100.0), Closed(100.0))
    if (x in domain) {
      val n = x.length

      val product = x.zipWithIndex.map { case (xi, i) =>
        val sum = (1 to 32).map { j =>
          val term = (2 ** j) * xi
          abs(term - round(term)) / (2 ** j)
        }.sum
        (1 + i + sum) ** (10.0 / pow(n, 1.2))
      }.product

      Some((10.0 / (n ** 2)) * product  - 1)
    } else None
  }

  def levy(x: List[Double]) = {
    val domain = Interval(Closed(-10.0), Closed(10.0))

    def w(i: Int) = 1 + (x(i) - 1) / 4

    if (x in domain) {
      val n = x.length
      val term1 = (sin(3 * pi * w(0))) ** 2

      val term2 = (0 until n - 1).map { i =>
        (w(i) - 1) ** 2 * (1 + sin(3 * pi * w(i) + 1) ** 2)
      }.sum

      val term3 = (w(n - 1) - 1) ** 2 * (1 + sin(2 * pi * w(n - 1)) ** 2)

      Some(term1 + term2 + term3)
    } else None
  }

  def matyas(x: List[Double]) = {
    val domain = Interval(Closed(-10.0), Closed(10.0))
    x match {
      case List(x1, x2) =>
        if (x in domain) Some(0.26 * (x1 ** 2 + x2 ** 2) - 0.48 * x1 * x2)
        else None
      case _ => None
    }
  }

  def maximum(x: List[Double]) = {
    x match {
      case List() => None
      case _      => Some(x.max)
    }
  }

  def minimum(x: List[Double]) = {
    x match {
      case List() => None
      case _      => Some(x.min)
    }
  }

  def michalewicz(x: List[Double]) = {
    val domain = Interval(Closed(0.0), Closed(pi))

    if (x in domain) {
      val m = 10.0
      Some(-x.zipWithIndex.map { case (xi, i) =>
        sin(xi) * (sin(((i + 1) * (xi ** 2)) / pi) ** (2 * m))
      }.sum)
    } else None
  }

  def nastyBenchmark(x: List[Double]) =
    Some(x.zipWithIndex.map { case(xi, i) => (xi - (i + 1)) ** 2 })

  def rastrigin(x: List[Double]) = {
    val domain = Interval(Closed(-5.12), Closed(5.12))
    if (x in domain)
      Some(10 * x.size + x.map(xi => xi ** 2 - 10 * cos(2 * pi * xi)).sum)
    else None
  }

  def rosenbrock(x: List[Double]) =
    Some((0 until x.length - 1).map(i => {
      val term1 = 100 * (x(i + 1) - x(1) ** 2) ** 2
      val term2 = (x(1) - 1) ** 2
      term1 + term2
    }).sum)

  def salomon(x: List[Double]) =
    spherical(x).map(sum => -cos(2 * pi * sqrt(sum)) + (0.1 * sqrt(sum)) + 1)

  def schaffer2(x: List[Double]) = x match {
    case List(_, _) => spherical(x).map(sum =>
      (sum ** 0.25) * (50 * (sum ** 0.1) + 1))
    case _          => None
  }

  def spherical(x: List[Double]) = Some(x.map(_ ** 2).sum)

  def step(x: List[Double]) = Some(x.map(xi => (floor(xi) + 0.5) ** 2).sum)

  def schwefel(x: List[Double]) = {
    val domain = Interval(Closed(-500.0), Closed(500.0))

    if (x in domain)
      Some(418.9829 * x.length - x.map(xi => xi * sin(sqrt(abs(xi)))).sum)
    else None
  }

  def shubert(x: List[Double]) = {
    val domain = Interval(Closed(-10.0), Closed(10.0))
    if ((x in domain) && (x.length == 2))
      Some(x.map(xi => (1 to 5).map(j => j * cos((j + 1) * xi + j)).sum).product)
    else None
  }

  def threeHumpCamelback(x: List[Double]) = {
    val domain = Interval(Closed(-5.0), Closed(5.0))
    x match {
      case List(x1, x2) =>
        if (x in domain)
          Some(2 * x1 ** 2 - 1.05 * x1 ** 4 + ((x1 ** 6) / 6) +
               x1 * x2 + x2 ** 2)
        else None
      case _ => None
    }
  }

  def vincent(x: List[Double]) = {
    val domain = Interval(Closed(0.25), Closed(10.0))
    if (x in domain) Some(-x.map(xi => sin(10.0 * log(xi))).sum)
    else None
  }

  def zakharov(x: List[Double]) = {
    val domain = Interval(Closed(-5.0), Closed(10.0))
    if (x in domain) {
      val term = x.zipWithIndex.map { case (xi, i) => 0.5 * i * xi }.sum
      spherical(x).map(sum => sum + term ** 2 + term ** 4)
    } else None
  }

}
