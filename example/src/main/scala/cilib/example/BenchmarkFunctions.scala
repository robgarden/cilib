package cilib
package example

import cilib.Functions._

import spire.math._
import spire.algebra._
import spire.implicits._

object BenchmarkFunctions {

  def main(args: Array[String]): Unit = {

    val x = List(1.0, 2.0, 3.0)
    val y = "%s(%s)\t\t%s".format("Spherical[Double]", x, spherical[Double](x))
    println(y)


    // dimension of input vector
    implicit val dim = JetDim(x.length)

    // helper method to convert List[Double] -> List[Jet[Double]]
    implicit class jetListOps(l: List[Double]) {
      def toJetList: List[Jet[Double]] = {
        l.zipWithIndex.map {
          case (li, i) => li + Jet.h[Double](i)
        }
      }
    }

    // create Jet input list
    val a = x.toJetList
    val b = "%s(%s)\t%s".format(
      "Spherical[Jet[Double]]", x, spherical[Jet[Double]](a)
    )
    val c= "%s(%s)\t%s".format(
      "Discus[Jet[Double]]", x, discus[Jet[Double]](a)
    )

    println(b)
    println(c)
  }
}
