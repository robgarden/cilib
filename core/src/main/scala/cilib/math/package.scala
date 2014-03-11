package cilib

import spire.math._
import spire.implicits._
import scala.language.implicitConversions

package object math {
	def sinD(x: Dual) = Dual(sin(x.r), x.d * cos(x.r))
	def cosD(x: Dual) = Dual(cos(x.r), x.d * -sin(x.r))
	def tanD(x: Dual) = Dual(tan(x.r), x.d / (cos(x.r) * cos(x.r)))

	def asinD(x: Dual) = Dual(asin(x.r), x.d / sqrt(1.0 - x.r ** 2))
	def acosD(x: Dual) = Dual(acos(x.r), x.d / (-sqrt(1 - x.r ** 2)))

	def absD(x: Dual) = if (x.r < 0) Dual(-x.r, -x.d) else x
	def expD(x: Dual) = Dual(exp(x.r), x.d * exp(x.r))
	def logD(x: Dual) = Dual(log(x.r), x.d / x.r)
	def sqrtD(x: Dual) = Dual(sqrt(x.r), x.d / (2.0 / sqrt(x.r)))

	def signumD(x: Dual) = Dual(signum(x.r), 0)
	def ceilD(x: Dual) = Dual(ceil(x.r), ceil(x.d))
	def floorD(x: Dual) = Dual(floor(x.r), floor(x.d))

	implicit def doubleToDual(x: Double) = Dual.zero(x)
}

