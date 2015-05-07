package cilib

import scalaz._
import Scalaz._

import spire.algebra._
import spire.math._
import spire.implicits._

object FunctionMetrics {

  def distance(a: Position[List, Double], b: Position[List, Double]) =
    sqrt((a - b).pos.map(_ ** 2).sum)

  def fdc(solutions: List[Position[List, Double]]) = {

    val fittest = solutions.reduceLeft((a, b) => Fitness.compare(a, b) run Min)

    val F: Maybe[List[Double]] = solutions.traverse(_.fit).map(_.map {
      case Valid(v) => v
      case Penalty(v, _) => v
    })

//    val F1 = solutions.traverse(_.fit).map(_.map(_.pos))

    val fbar: Maybe[Double] = F.map(l => l.sum / l.length)

    val dstar = solutions.map(s => distance(s, fittest))

    val dbar: Double = dstar.sum / dstar.length

    val zipped: Maybe[List[(Double, Double)]] = F.map(_ zip dstar)

    val numer: Maybe[Double] = zipped.flatMap(zi => zi.traverse {
      case (fi, di) => fbar.map(fb => (fi - fb) * (di - dbar))
    }).map(_.sum)

    val denomTerm1: Maybe[Double] =
      F.flatMap(_.traverse(fi => fbar.map(fb => (fi - fb) ** 2))).map(l => sqrt(l.sum))
    val denomTerm2: Double = sqrt(dstar.map(di => (di - dbar) ** 2).sum)

    val denom = denomTerm1.map(_ * denomTerm2)

    for {
      n <- numer
      d <- denom
    } yield n / d

  }

}
