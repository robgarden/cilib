package cilib.functions.continuous

import spire.implicits._
import spire.math._

import cilib.Solution
import cilib.math._

trait ContinuousFunction {
	def apply(x: Solution): Double
}

trait Differentiable {
	def f(x: Dual): Dual
	def gradient(x: Solution): Solution = x.map(f(_).d)
}

object AbsoluteValue extends ContinuousFunction with Differentiable {
	def f(x: Dual) = absD(x)
	def apply(x: Solution) = x.map(abs(_)).sum
}

object Rosenbrock extends ContinuousFunction {
	def apply(x: Solution) = x.sliding(2).map {
			case Vector(xi, xi1) => (1 - xi) ** 2 + (100 * (xi1 - xi ** 2)) ** 2
	}.sum
}

object Rastrigin extends ContinuousFunction with Differentiable {
	def f(x: Dual) = x ** 2 - 10 * cos(2 * pi * x)
	def apply(x: Solution) = 10 * x.size + x.map(f(_).r).sum
}

object Salomon extends ContinuousFunction {
	def apply(x: Solution): Double = {
		val sqrtSquares = sqrt(Sphere(x))
		-cos(2 * pi * sqrtSquares) + (0.1 * sqrtSquares) + 1
	}
}

object Sphere extends ContinuousFunction with Differentiable {
	def f(x: Dual) = x ** 2
	def apply(x: Solution) = x.map(f(_).r).sum
}

object Step extends ContinuousFunction {
	def apply(x: Solution) = x.map(xi => (floor(xi) + 0.5) ** 2).sum
}
