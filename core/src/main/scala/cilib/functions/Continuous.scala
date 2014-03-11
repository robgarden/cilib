package cilib.functions.continuous

import spire.implicits._
import spire.math._

import cilib.Solution
import cilib.math.Dual
import cilib.math.DualImplicits._

trait ContinuousFunction {
	def apply(x: Solution): Dual
}

object AbsoluteValue extends ContinuousFunction {
	def apply(x: Solution) = 
		x.zipWithIndex.map(xi => absD(Dual.one(xi._1, xi._2, x.size))).reduce(_ + _)
}

object Rosenbrock extends ContinuousFunction {
	def apply(x: Solution) = x.zipWithIndex.sliding(2).map {
			case Vector(xi, xi1) => {
				val xid = Dual.one(xi._1, xi._2, x.size)
				val xi1d = Dual.one(xi1._1, xi1._2, x.size)
				((-xid + 1) ** 2) + ((xi1d - (xid ** 2)) * 100) ** 2
			}
	}.reduce(_ + _)
}

object Rastrigin extends ContinuousFunction {
	def apply(x: Solution) = {
		x.zipWithIndex.map(xi => Dual.one(xi._1, xi._2, x.size)).map { xi =>
			xi ** 2 - cosD(xi * 2 * pi) * 10
		}.reduce(_ + _) * (10 + x.size)
	}
}

// object Salomon extends ContinuousFunction {
// 	def apply(x: Solution): Double = {
// 		val sqrtSquares = sqrt(Sphere(x))
// 		-cos(2 * pi * sqrtSquares) + (0.1 * sqrtSquares) + 1
// 	}
// }

object Sphere extends ContinuousFunction {
	// def f(x: Dual) = x ** 2
	def apply(x: Solution) = 
		x.zipWithIndex.map(xi => Dual.one(xi._1, xi._2, x.size) ** 2).reduce(_ + _)
}

// object Step extends ContinuousFunction {
// 	def apply(x: Solution) = x.map(xi => (floor(xi) + 0.5) ** 2).sum
// }
