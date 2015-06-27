package cilib

import scalaz._
import Scalaz._

import spire.algebra._
import spire.math._
import spire.implicits._

object FunctionMetrics {

  def distance(a: Position[List, Double], b: Position[List, Double]) =
    sqrt((a - b).pos.map(_ ** 2).sum)

  def fdc(opt: Opt)(solutions: List[Position[List, Double]]): Maybe[Double] = {
    val fitnesses: Maybe[List[Fit]] = solutions.traverse(_.fit)

    val F = for {
      fit <- fitnesses
      v = fit.map {
        case Valid(v) => v
        case Penalty(v, _) => v
      }
    } yield v

    val fittest = solutions.reduceLeft((a, b) => Fitness.compare(a, b) run opt)

    for {
      f      <- F
      fbar   = f.sum / f.length
      dstar  = solutions.map(s => distance(s, fittest))
      dbar   = dstar.sum / dstar.length
      zipped = f zip dstar
      numer  = zipped.map { case (fi, di) => (fi - fbar) * (di - dbar) }.sum
      denom1 = sqrt(f.map(fi => (fi - fbar) ** 2).sum)
      denom2 = sqrt(dstar.map(di => (di - dbar) ** 2).sum)
    } yield numer / (denom1 * denom2)

  }

}
