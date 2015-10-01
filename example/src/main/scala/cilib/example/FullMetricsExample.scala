package cilib
package example

import benchmarks.Benchmarks._
import scalaz._
import Scalaz._

object FullMetricsExample {

  def main(args: Array[String]): Unit = {

    // Create RNG with seed '0'
    // Fixing the seed allows experiments to be reproduced exactly
    val rng = RNG init 0

    val problems = Map(
      "Spherical" -> Problems.spherical,
      "Ackley" -> Problems.ackley,
      "Absolute" -> Problems.absolute
    )

    // Define bounds, domain, step size for the walk, and number of steps in the walk
    val bounds = Interval(closed(-10.0), closed(10.0))
    val domain = bounds^10
    val stepSize = (bounds.upper.value - bounds.lower.value) * (1.0 / 32)
    val numSteps = 1000

    // Create a random walk using the domain of the problem, number of steps, and step size
    val walk = RandomWalks.progressive(domain, numSteps, stepSize)

    // Define gradient metric
    val gradientAverageMetric = FunctionMetrics.gradientAvg(stepSize, domain)

    println()
    println(s"Average gradient on ${problems.length} problems")

    problems.foreach {
      case (name, problem) =>

        val solutions = for {
          points <- walk
          steps  =  points.map(p => Step.evalF(p))
          sols   <- steps.traverse(step => (step run Min)(problem))
        } yield toSized2And(sols)

        val gradientAverage: RVar[Maybe[Double]] = solutions.map(_.flatMap(gradientAverageMetric))

        val a = (0 until 10).toList.map(_ => gradientAverage run rng).traverse(f1 => f1._2)
        val avg = a.map(ai => ai.sum / ai.length)

        println(s"$name: $avg")
    }
    println()
  }

}
