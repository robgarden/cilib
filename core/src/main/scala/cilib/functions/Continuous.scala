package cilib.functions.continuous

import spire.implicits._
import spire.math._

import cilib.Solution

trait ContinuousFunction {
	def apply(x: Solution): Double
}

object AbsoluteValue extends ContinuousFunction {
	def apply(x: Solution) = x.map(abs(_)).sum
}

object Rosenbrock extends ContinuousFunction {
	def apply(x: Solution) = x.sliding(2).map { 
			case Vector(xi, xi1) => (1 - xi) ** 2 + (100 * (xi1 - xi ** 2)) ** 2 
	}.sum
}

object Rastrigin extends ContinuousFunction {
	def apply(x: Solution) = 10 * x.size + x.map(xi => xi ** 2 - 10 * cos(2 * pi * xi)).sum
}

object Salomon extends ContinuousFunction {
	def apply(x: Solution): Double = {
		val sqrtSquares = sqrt(Sphere(x))
		-cos(2 * pi * sqrtSquares) + (0.1 * sqrtSquares) + 1
	}
}

object Sphere extends ContinuousFunction {
	def apply(x: Solution) = x.map(_ ** 2).sum
}

object Step extends ContinuousFunction {
	def apply(x: Solution) = x.map(xi => (floor(xi) + 0.5) ** 2).sum
}
