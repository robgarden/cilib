package cilib
package example

import cilib._

import scalaz.std.list._

import spire.math._
import spire.algebra._
import spire.implicits._

object AutomaticDifferentiation {

  def main(args: Array[String]): Unit = {

    // // use dual numbers with 2 infinitesimal dimensions
    // implicit val dim = JetDim(2)

    // implicit val jetMonoid: scalaz.Monoid[Jet[Double]] = new scalaz.Monoid[Jet[Double]] {
    //   def zero = scalaz.Monoid[Double].zero
    //   def append(x: Jet[Double], y: => Jet[Double]): Jet[Double] = scalaz.Monoid[Double].append(x.doubleValue(), y.doubleValue())
    // }

    // val x = 1.5 + Jet.h[Double](0)
    // val y = 2.7 + Jet.h[Double](1)
    // val z = List(x, y)

    // println("val x = " + x)
    // println("val y = " + y)
    // println("val z = List(x, y)")

    // println("Non-bounded benchmark functions")
    // println("===============================")
    // println("AbsoluteValue(z):\t" + Functions.absoluteValue(z))
    // println("Spherical(z):\t\t" + Functions.spherical(z))

    // // for use with intervals
    // // there must be a better way... (maybe spire.math.Interval?)
    // implicit val jetIsReal = new IsReal[Jet[Double]] {
    //   def floor(x: Jet[Double]) = x.floor
    //   def round(x: Jet[Double]) = x.round
    //   def ceil(x: Jet[Double]) = x.ceil
    //   def isWhole(x: Jet[Double]) = x.isWhole
    //   def toDouble(x: Jet[Double]) = x.toDouble
    //   def compare(x: Jet[Double], y: Jet[Double]) = Order[Double].compare(x.real, y.real)
    //   def abs(x: Jet[Double]) = x.abs
    //   def signum(x: Jet[Double]) = x.signum
    // }

    // println("Bounded benchmark functions")
    // println("===========================")
    // println("Ackley(z):\t\t" + Functions.ackley(z))
    // println("Beale(z):\t\t" + Functions.beale(z))

    // val lin = ActivationFunctions.linear[Jet[Double]]()
    // val sig = ActivationFunctions.sigmoid[Jet[Double]]()

    // println("Activation functions")
    // println("====================")
    // println("Linear(x):\t\t" + lin(x))
    // println("Sigmoid(x):\t\t" + sig(x))
  }
}