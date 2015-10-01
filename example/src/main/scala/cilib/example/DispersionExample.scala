package cilib
package example

import benchmarks.Benchmarks._
import scalaz._
import Scalaz._

object DispersionExample {

  def main(args: Array[String]): Unit = {

    val sum = Problems.spherical

    val bounds = Interval(closed(-10.0), closed(10.0))
    val domain = bounds^10
    val stepSize = (bounds.upper.value - bounds.lower.value) * (1.0 / 32)

    // val walk = Position.createPositions(domain, 1000)
    val walk: RVar[List[Position[List,Double]]] = RandomWalks.progressiveManhattan(domain, 1000, stepSize)

    val solutions: RVar[List[Position[List, Double]]] = for {
      points <- walk
      steps  =  points.map(p => Step.evalF(p))
      sols   <- steps.traverse(step => (step run Min)(sum))
    } yield sols

    val dispersion = FunctionMetrics.dispersion(0.1, Min, domain)
    val metric = solutions.map(dispersion)

    val a = (0 until 10).toList.map(_ => metric run RNG.fromTime).traverse(f1 => f1._2)
    val avg = a.map(ai => ai.sum / ai.length)
    println(avg)

  }

}
