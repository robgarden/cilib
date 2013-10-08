package net.sourceforge.cilib.measurement.functionmetric

import fj.F;
import net.sourceforge.cilib.functions.ContinuousFunction;
import net.sourceforge.cilib.`type`.types.container.Vector;
/**
 * Wraps a CILIB function to work with a Point
 *
 */
class ScalaFunctionWrapper(val func: F[Vector, Double]) {
  def apply(x: Array[Double]): Double = {
    val builder = Vector.newBuilder()
    x foreach { xi =>
      builder.add(xi)
    }
    func.f(builder.build())
  }
}
