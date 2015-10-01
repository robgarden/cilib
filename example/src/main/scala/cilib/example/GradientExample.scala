package cilib
package example

import benchmarks.Benchmarks._
import scalaz._
import Scalaz._

/**
 * The following example runs the 'Gradient Average' measure on the Spherical problem
 * using a collection of solutions sampled from a Manhattan progressive random walk.
 * The computation is built up inside an 'RVar' container, which is finally run using the RNG.
 * While building the computation, various steps return an optional data type to cater for
 * incorrect type conversions.
 * This optional type is then carried through as the final result in the computation.
 * If any step fails, the computation won't crash with a runtime error, instead the result
 * will simply be empty.
 */
object GradientExample {

  def main(args: Array[String]): Unit = {

    // Define Problem
    val sphere = Problems.spherical

    // Define bounds, domain, step size for the walk, and number of steps in the walk
    val bounds = Interval(closed(-10.0), closed(10.0))
    val domain = bounds^10
    val stepSize = (bounds.upper.value - bounds.lower.value) * (1.0 / 32)
    val numSteps = 1000

    // Create a manhattan random walk using the domain of the problem, number of steps, and step size
    val walk: RVar[List[Position[List,Double]]] = RandomWalks.progressiveManhattan(domain, numSteps, stepSize)

    // Average Gradient requires that positions in the walk have a fitness calculated using the problem.
    // This step evaluates each position in the walk and assigns it a fitness.
    // Note how the types of 'walk' and 'solutions' stays the same, however 'solutions' now contains
    // positions with an associated fitness.
    //
    // N.B: This step can result in a runtime error if the dimension of the positions does not match
    // the expected dimension of the problem!
    // See 'Problems.scala'
    val solutions: RVar[List[Position[List, Double]]] = for {
      points <- walk
      steps  =  points.map(p => Step.evalF(p))
      sols   <- steps.traverse(step => (step run Min)(sphere))
    } yield sols

    // Now, since gradient requires at least 2 positions in the walk, we need to convert the list
    // of positions to a type that the metric expects. We use 'Sized2And' to denote a list that has
    // at least 2 elements.
    // Notice that the function 'toSized2And' returns a 'Maybe[A]', based on whether the list can indeed
    // be converted to a 'Sized2And' type
    // This 'Maybe' is carried through the rest of the computation
    val solutionsSized2And: RVar[Maybe[Sized2And[List, Position[List, Double]]]] = solutions.map(toSized2And)

    // The average gradient matric needs to be created with knowledge of the walk, in this case
    // the step size and the domain of the problem.
    val gradientAverageMetric: Sized2And[List, Position[List, Double]] => Maybe[Double] = FunctionMetrics.gradientAvg(stepSize, domain)

    // Now apply the metric to the solutions.
    val gradientAverage: RVar[Maybe[Double]] = solutionsSized2And.map(_.flatMap(gradientAverageMetric))

    // Finally, we run the metric 10 times using 'RNG.fromTime' and print the average
    val a = (0 until 10).toList.map(_ => gradientAverage run RNG.fromTime).traverse(f1 => f1._2)
    val avg = a.map(ai => ai.sum / ai.length)
    println(avg)

  }

}
