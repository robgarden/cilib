package cilib.functions.decorators

import spire.implicits._
import spire.math._

import cilib.Solution
import cilib.functions.continuous.ContinuousFunction

trait Decorator extends ContinuousFunction

class Shifted(f: ContinuousFunction, horizontal: Double, vertical: Double) 
		extends Decorator {

	def apply(x: Solution) = f(x.map(_ - horizontal)) + vertical
}

class Scaled(f: ContinuousFunction, horizontal: Double, vertical: Double)
	extends Decorator {

	def apply(x: Solution) = f(x.map(_ * horizontal)) * vertical
}

class Composite(f: ContinuousFunction, g: ContinuousFunction) extends Decorator {
	def apply(x: Solution) = f(Vector(g(x)))
}