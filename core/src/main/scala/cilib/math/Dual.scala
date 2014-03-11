package cilib.math

import spire.math._
import spire.implicits._

object Dual {
	def apply(r: Double, d: Double) = new Dual(r, d)
	def one(x: Double): Dual = Dual(x, 1)
	def zero(x: Double): Dual = Dual(x, 0)
}

class Dual(val r: Double, val d: Double) {
	def +(that: Dual) = Dual(r + that.r, d + that.d)
	def -(that: Dual) = Dual(r - that.r, d - that.d)
	def *(that: Dual) = Dual(r * that.r, r * that.d + d * that.r)
	def /(that: Dual) = Dual(r / that.r, (d * that.r - r * that.d) / (that.r ** 2))
	def **(that: Dual) = Dual(pow(r, that.r), if (r == 0) 0 else that.r * pow(r, that.r) * d / r)
	def unary_-() = Dual(-r, -d)

	def isEqual(that: Dual) = r == that.r && d == that.d
	override def toString = s"<$r, $d>"
}