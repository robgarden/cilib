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
    val e = is1._3
    e
  }

  def infoContent(e: Double, x: Sized2And[List, Double]) = {
    val s = S(e, x)

    def hash(p: Int, q: Int): Int = {
      val x = -p + 2 * q
      if (x < 0) x + 3
      else x + 2
    }

    val grouped = s.sliding(2).toList
      .filter  { case Seq(p, q) => p != q }
      .map     { case Seq(p, q) => hash(p, q) }
      .groupBy { x => x }

    val entropy = grouped.values.toList.map(_.length).map {
      ent =>
        val prob = ent.toDouble / (s.length - 1)
        prob * (math.log(prob) / math.log(6))
    }

    -entropy.sum
  }

  def fem(x: Sized3And[List, Position[List, Double]]): Maybe[Double] = {
    val points = x.a :: x.b :: x.c :: x.rest.toList
    val fitnesses: Maybe[List[Fit]] = points.traverse(_.fit)

    val f: Maybe[List[Double]] = for {
      l <- fitnesses
      v = l.map(_ match {
        case Valid(v) => v
        case Penalty(v,_) => v
      })
    } yield v

    val increment = 0.05
    val numEpsilons = (1.0 / increment).toInt + 1

    def getEpsilons(e: List[Double], mult: Double, i: Int, es: Double): List[Double] =
      if (i < numEpsilons) getEpsilons(e.updated(i, es * mult), mult + increment, i + 1, es)
      else e

    for {
      fi          <- f
      x2          <- toSized2And(fi)
      epsilonStar =  infoStability(x2)
      epsilons    =  getEpsilons(List.fill(numEpsilons)(0), 0.0, 0, epsilonStar)
      es          =  epsilons.map(e => infoContent(e, x2))
    } yield es.max

  }

  def main(args: Array[String]): Unit = {
    val sum = Problems.sphere

    val bounds = Interval(closed(-100.0), closed(100.0))

    val points: RVar[List[Position[List,Double]]] = RandomWalks.progressive(bounds^1, 1000, 20)
    // val points = Position.createPositions(bounds^10, 500)

    val f: RVar[Maybe[Double]] = for {
      p <- points
      s <- p.traverse(_.eval(sum))
    } yield toSized3And(s).flatMap(fem)

    // val solutions1: RVar[Maybe[Sized3And[List, Position[List, Double]]]] = solutions.map(toSized3And)

    // val f: RVar[Maybe[Double]] = solutions1.map(_.flatMap(fem))

    val a = (0 until 1).toList.map(_ => f run RNG.fromTime).traverse(f1 => f1._2)
    val avg = a.map(ai => ai.sum / ai.length)
    println(avg)
  }

}
