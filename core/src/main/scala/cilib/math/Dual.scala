package cilib.math

import cilib.Solution
import cilib.DualSolution
import language.implicitConversions
import spire.math._
import spire.implicits._

object Dual {

	def apply(r: Double, d: Vector[Double] = Vector(0)) = new Dual(r, d)

	def one(x: Double, index: Int = 0, size: Int = 1) =
		Dual(x, Vector.tabulate(size)(i => if (i == index) 1.0 else 0.0))

	def zero(x: Double) = Dual(x, Vector(0))

	def solution(x: Solution) = x.zipWithIndex.map(xi => Dual.one(xi._1, xi._2, x.size))

	object operations {
		def sinD(x: Dual) = Dual(sin(x.r), x.d :* cos(x.r))
		def cosD(x: Dual) = Dual(cos(x.r), x.d :* -sin(x.r))
		def tanD(x: Dual) = Dual(tan(x.r), x.d :/ (cos(x.r) ** 2))

		def asinD(x: Dual) = Dual(asin(x.r), x.d :/ sqrt(1.0 - x.r ** 2))
		def acosD(x: Dual) = Dual(acos(x.r), x.d :/ -sqrt(1.0 - x.r ** 2))

		def absD(x: Dual) = if (x.r < 0) Dual(-x.r, -x.d) else x
		def expD(x: Dual) = Dual(exp(x.r), x.d :* exp(x.r))
		def logD(x: Dual) = Dual(log(x.r), x.d :/ x.r)
		def sqrtD(x: Dual) = Dual(sqrt(x.r), x.d :/ (2.0 / sqrt(x.r)))
	}

	object implicits {
		implicit def double2Dual(x: Double) = Dual.zero(x)
	}
}

class Dual(val r: Double, val d: Vector[Double]) {
	def +(that: Dual) = Dual(r + that.r, d + that.d)
	def -(that: Dual) = Dual(r - that.r, d - that.d)
	def *(that: Dual) = Dual(r * that.r, (that.d :* r) + (d :* that.r))
	def /(that: Dual) = Dual(r / that.r, ((d :* that.r) - (that.d :* r)) :/ (that.r ** 2))
	def unary_-() = Dual(-r, -d)
	def **(that: Dual) = Dual(pow(r, that.r),
		if (r == 0) Vector.tabulate(d.size)(x => 0.0) else (d :* that.r * pow(r, that.r)) :/ r)

	override def toString = s"<$r, $d>"
}