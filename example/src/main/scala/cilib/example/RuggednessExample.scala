package cilib
package example

import benchmarks.Benchmarks._
import scalaz._
import Scalaz._

/**
 * The following example runs the 'FEM' or ruggedness measure on the Spherical problem
 * using a collection of solutions sampled from a progressive random walk.
 * The computation is built up inside an 'RVar' container, which is finally run using the RNG.
 * While building the computation, various steps return an optional data type to cater for
 * incorrect type conversions.
 * This optional type is then carried through as the final result in the computation.
 * If any step fails, the computation won't crash with a runtime error, instead the result
 * will simply be empty.
 */
object RuggednessExample {

  def main(args: Array[String]): Unit = {

    // Define Problem
    val sphere = Problems.spherical

    // Define bounds, domain, step size for the walk, and number of steps in the walk
    val bounds = Interval(closed(-10.0), closed(10.0))
    val domain = bounds^10
    val stepSize = (bounds.upper.value - bounds.lower.value) * (1.0 / 32)
    val numSteps = 1000

    // Create a random walk using the domain of the problem, number of steps, and step size
    val walk: RVar[List[Position[List,Double]]] = RandomWalks.progressive(domain, numSteps, stepSize)

    // Ruggedness requires that positions in the walk have a fitness calculated using the problem.
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

    // Now, since ruggedness requires at least 3 positions in the walk, we need to convert the list
    // of positions to a type that the metric expects. We use 'Sized3And' to denote a list that has
    // at least 3 elements.
    // Notice that the function 'toSized3And' returns a 'Maybe[A]', based on whether the list can indeed
    // be converted to a 'Sized3And' type
    // This 'Maybe' is carried through the rest of the computation
    val solutionsSized3And: RVar[Maybe[Sized3And[List, Position[List, Double]]]] = solutions.map(toSized3And)

    // Now apply the 'FEM' function metric to the solutions.
    // Notice how the return type of 'FEM' is 'Maybe[Double]'.
    // This is to cater for the case where the positions do not have a fitness. In this case,
    // 'FEM' will return 'Empty'
    // If the positions do have a fitness, 'FEM' will return 'Just[Double]'
    // Also notice how 'flatMap' collapses the 'Maybe' from 'toSized3And' and from 'FEM' into a single
    // 'Maybe'
    val ruggedness: RVar[Maybe[Double]] = solutionsSized3And.map(_.flatMap(FunctionMetrics.fem))

    // Finally, we run the metric 10 times using 'RNG.fromTime' and print the average
    val a = (0 until 10).toList.map(_ => ruggedness run RNG.fromTime).traverse(f1 => f1._2)
    val avg = a.map(ai => ai.sum / ai.length)
    println(avg)

  }

}
