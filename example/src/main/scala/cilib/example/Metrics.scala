package cilib
package example

import scalaz._
import scalaz._

object MetricsExample {

  def main(args: Array[String]): Unit = {

    val bounds = Interval(closed(0.0), closed(10.0))

    val out = RandomWalks.progressiveManhattan(bounds^2, 50, 1.0)

    ///println(out run RNG.fromTime)

     (out run RNG.fromTime)._2.foreach { l =>
       println(l.pos(0) + "," + l.pos(1))
     }
  }

}
