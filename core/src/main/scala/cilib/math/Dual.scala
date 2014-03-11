package cilib.math

import spire.math._
import spire.implicits._

import scala.language.implicitConversions

object DualImplicits {
	implicit def vectorD2Vec(x: Vector[Double]) = new Vec(x)
	implicit def vectorI2Vec(x: Vector[Int]) = new Vec(x.map(_.toDouble))

	def sinD(x: Dual) = Dual(sin(x.r), x.d.map(_ * cos(x.r)))
	def cosD(x: Dual) = Dual(cos(x.r), x.d.map(_ * -sin(x.r)))
	def tanD(x: Dual) = Dual(tan(x.r), x.d div (cos(x.r) * cos(x.r)))

	def asinD(x: Dual) = Dual(asin(x.r), x.d div sqrt(1.0 - x.r ** 2))
	def acosD(x: Dual) = Dual(acos(x.r), x.d div (-sqrt(1 - x.r ** 2)))

	def absD(x: Dual) = if (x.r < 0) Dual(-x.r, -x.d) else x
	def expD(x: Dual) = Dual(exp(x.r), x.d.map(_ * exp(x.r)))
	def logD(x: Dual) = Dual(log(x.r), x.d.map(_ / x.r))
	def sqrtD(x: Dual) = Dual(sqrt(x.r), x.d div (2.0 / sqrt(x.r)))

	implicit def doubleToDual(x: Double) = Dual.zero(x)
}

object Dual {
	def apply(r: Double, d: Vector[Double]) = new Dual(r, d)
	def one(x: Double, index: Int = 0, size: Int = 1) = 
		Dual(x, Vector.tabulate(size)(i => if (i == index) 1.0 else 0.0))
	def zero(x: Double, size: Int = 1): Dual = Dual(x, Vector.tabulate(size)(x => 0.0))
}

class Dual(val r: Double, val d: Vector[Double]) {
	import cilib.math.DualImplicits._
	def +(that: Dual) = Dual(r + that.r, d + that.d)
	def -(that: Dual) = Dual(r - that.r, d - that.d)
	def *(that: Dual) = Dual(r * that.r, (that.d mul r) + (d mul that.r))
	def /(that: Dual) = Dual(r / that.r, ((d mul that.r) - (that.d :* r)) div (that.r * that.r))
	def **(that: Dual) = Dual(pow(r, that.r), 
		if (r == 0) Vector.tabulate(d.size)(x => 0.0) else (d mul that.r * pow(r, that.r)) div r)
	def unary_-() = Dual(-r, -d)

	def isEqual(that: Dual) = r == that.r && d == that.d
	override def toString = s"<$r, $d>"
}

class Vec(val x: Vector[Double]) {
	def fix(that: Vec): Vector[Double] =
		if (that.x.size == this.x.size) that.x 
		else that.x ++ Vector.tabulate(abs(this.x.size - that.x.size))(x => 0.0)

	def +(that: Vec): Vector[Double] = this.x.zip(fix(that)).map { case (a, b) => a + b }
	def -(that: Vec): Vector[Double] = this.x.zip(fix(that)).map { case (a, b) => a - b }
	def *(that: Vec): Vector[Double] = this.x.zip(fix(that)).map { case (a, b) => a * b }
	def /(that: Vec): Vector[Double] = this.x.zip(fix(that)).map { case (a, b) => a / b }
	def mul(s: Double): Vector[Double] = this.x.map(_ * s)
	def div(s: Double): Vector[Double] = this.x.map(_ / s)
	def unary_-() = this.x.map(-_)
}
