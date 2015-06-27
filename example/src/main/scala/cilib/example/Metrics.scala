package cilib
package example

import benchmarks.Benchmarks._
import scalaz._
import Scalaz._

object MetricsExample {

  def S(e: Double, x: Sized2And[List, Double]): List[Int] = {
    def calcS(a: Double, b: Double): Int =
      if (b - a < -e) -1
      else if (math.abs(b - a) <= e) 0
      else 1

    val points = x.a :: x.b :: x.rest.toList
    points.sliding(2).toList.map {
      case Seq(a, b) => calcS(a, b)
    }
  }

  def isLandscapeFlat(x: List[Int]) = x.forall(_ == 0)

  def infoStability(x: Sized2And[List, Double]): Double = {
    // eb, es, e, eo
    type IS = (Double, Double, Double, Int)

    def fbo(is: IS): IS = is match {
      case (eb, es, e, eo) =>
        val s = S(e, x)
        if (isLandscapeFlat(s)) (eb, es / eb, e, eo)
        else fbo((eb, es * eb, es, eo + 1))
    }

    val is: IS = fbo((10, 10, 0, 0))

    val smallestStep = 0.01 * math.pow(10, is._4.toDouble)

    def nfp(is: IS): IS =
      is match {
        case (eb, es, e, eo) =>
          val s = S(e, x)
          val flat = isLandscapeFlat(s)
          if (flat) {
            if (es <= smallestStep) {
              is
            } else {
              val e1 = e - es
              val es1 = es / eb
              val e2 = e1 + es1
              nfp((eb, es1, e2, eo))
            }
          } else nfp((eb, es, e + es, eo))
      }

    val is1 = nfp(is)
    val e = is._3
    e
  }

  def computeHash(p: Int, q: Int): Int = {
    val x = -p + 2 * q
    if (x < 0) x + 3
    else x + 2
  }

  def infoContent(e: Double, x: Sized2And[List, Double]) = {
    val s = S(e, x)

    def cs(n: List[Int], p: Int): List[Int] =
      if (p < s.length - 1) {
        val q = p + 1
        if (s(p) != s(q)) {
          val hash = computeHash(s(p), s(q))
          cs(n.updated(hash, n(hash) + 1), p + 1)
        }
        else cs(n, p + 1)
      } else n

    val c: List[Int] = cs(List.fill(6)(0), 0)
    val n = s.length - 1

    def ent(entropy: Double, i: Int): Double =
      if (i < 6) {
        val prob = c(i).toDouble / n
        if (prob != 0) {
          val entropy1 = prob * (math.log(prob) / math.log(6))
          ent(entropy + entropy1, i + 1)
        } else ent(entropy, i + 1)
      } else entropy

    val entropy = ent(0, 0)
    -entropy
  }

  def fem(x: Sized3And[List, Position[List, Double]]): Maybe[Double] = {
    val points = x.a :: x.b :: x.c :: x.rest.toList
    val fitnesses: Maybe[List[Fit]] = points.traverse(_.fit)

    val f: Maybe[List[Double]] = for {
      l <- fitnesses
    } yield l.map(_ match {
      case Valid(v) => v
      case Penalty(v,_) => v
    })

    val x2: Maybe[Sized2And[List,Double]] = f.flatMap(toSized2And(_))

    val epsilonStar: Maybe[Double] = x2.map(infoStability(_))
    val increment = 0.05
    val numEpsilons = (1.0 / increment).toInt + 1

    def getEpsilons(e: List[Double], mult: Double, i: Int, es: Double): List[Double] =
      if (i < numEpsilons) getEpsilons(e.updated(i, es * mult), mult + increment, i + 1, es)
      else e

    val epsilons: Maybe[List[Double]] = epsilonStar.map(getEpsilons(List.fill(numEpsilons)(0), 0.0, 0, _))

    for {
      x21 <- x2
      es <- epsilons
      newEs = es.map(e => infoContent(e, x21))
    } yield newEs.max

  }

  def main(args: Array[String]): Unit = {
    val sum = Problems.ackley

    val bounds = Interval(closed(-32.0), closed(32.0))

    val points: RVar[List[Position[List,Double]]] = RandomWalks.progressive(bounds^10, 1000, 6.4)

    val solutions: RVar[List[Position[List, Double]]] = points.flatMap(_.traverse(_.eval(sum)))

    val solutions1: RVar[Maybe[Sized3And[List, Position[List, Double]]]] = solutions.map(toSized3And)

    val f: RVar[Maybe[Double]] = solutions1.map(_.flatMap(fem))

    val a = (0 to 100).toList.map(_ => f run RNG.fromTime).traverse(f1 => f1._2)
    val avg = a.map(ai => ai.sum / ai.length)
    println(avg)
    // println(f run RNG.fromTime)
  }

}
