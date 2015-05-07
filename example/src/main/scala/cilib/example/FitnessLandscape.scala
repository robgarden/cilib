package cilib
package example

import scalaz._
import Scalaz._

object FitnessLandscape {

  def main(args: Array[String]): Unit = {

    val bounds = Interval(closed(0.0), closed(10.0))
    val problem = Unconstrained((x: List[Double]) => Valid(x.map(xi => xi * xi).sum))

    val samples = Position.createPositions(bounds^10, 1000)
    val solutions = samples.flatMap(_.traverse(s => s.eval(problem)))

    println(solutions.map(FunctionMetrics.fdc).run(RNG.fromTime))
  }

}
