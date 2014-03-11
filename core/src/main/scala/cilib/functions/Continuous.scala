package cilib.functions.continuous

import spire.math._
import spire.implicits._

import cilib.math._
import cilib.Solution

trait ContinuousFunction {
	def apply(x: Solution): Double
}

trait Differentiable {
	def f(x: Dual): Dual
	def gradient(x: Solution) = x.map(xi => f(Dual(xi, 1)).d)
}

object AbsoluteValue extends ContinuousFunction with Differentiable {
	def f(x: Dual) = absD(x)
	def apply(x: Solution) = x.map(xi => f(Dual.one(xi)).r).sum
}

// object Michalewicz extends ContinuousFunction with Differentiable {
// 	def indexedF(x: Dual, i: Int) = sinD(x) * (sinD(((x ** 2) * i) / pi) ** 20)
// 	def apply(x: Solution) = x.map(xi => f(Dual.one(xi)).r).sum
// 	def gradient(x: Solution) = x.
// }

object Rastrigin extends ContinuousFunction with Differentiable {
	def f(x: Dual) = x ** 2 - cosD(x * pi * 2) * 10
	def apply(x: Solution) = x.map(xi => f(Dual.one(xi)).r).sum
}

object Rosenbrock extends ContinuousFunction {
	def apply(x: Solution) = x.sliding(2).map {
			case Vector(xi, xi1) => (1 - xi) ** 2 + (100 * (xi1 - xi ** 2)) ** 2
	}.sum
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

object Schwefel226 extends ContinuousFunction with Differentiable {
	def f(x: Dual) = x * sinD(sqrtD(absD(x)))
	def apply(x: Solution) = x.map(xi => f(Dual.one(xi)).r).sum
}

// The sin (multimodal) functions

object EqualMaxima extends ContinuousFunction with Differentiable {
	def f(x: Dual) = sinD(x * 5 * pi) ** 6
	def apply(x: Solution) = x.map(xi => f(Dual.one(xi)).r).sum
}

object EvenDecreasingMaxima extends ContinuousFunction with Differentiable {
	def f(x: Dual): Dual = {
		val expTerm = expD((logD(2) / logD(10)) * -2 * (((x - 0.01) / 0.8) ** 2))
		val sinTerm = sinD(x * 5 * pi) ** 6
		expTerm * sinTerm
	}

	def apply(x: Solution) = x.map(xi => f(Dual.one(xi)).r).sum
}

object UnevenDecreasingMaxima extends ContinuousFunction with Differentiable {
	def f(x: Dual): Dual = {
		val expTerm = expD((logD(2)/logD(10)) * -2 * (((x - 0.08) / 0.854) ** 2))
		val sinTerm = sinD(((x ** 0.75) * 5 * pi - 0.05)) ** 6
		expTerm * sinTerm
	}

	def apply(x: Solution) = x.map(xi => f(Dual.one(xi)).r).sum
}

object UnevenEqualMaxima extends ContinuousFunction with Differentiable {
	def f(x: Dual) = sinD(((x ** 0.75) * 5 * pi - 0.05)) ** 6
	def apply(x: Solution) = x.map(xi => f(Dual.one(xi)).r).sum
}

object Vincent extends ContinuousFunction with Differentiable {
	def f(x: Dual) = sinD(logD(x) * 10)
	def apply(x: Solution) = x.map(xi => f(Dual.one(xi)).r).sum
}