// package cilib.functions.activation

// import spire.implicits._
// import spire.math._

// import cilib.Solution

// trait ActivationFunction {
// 	def apply(x: Double): Double
// 	def gradient(x: Double): Double
// 	val lowerActiveRange: Double = -sqrt(3)
// 	val upperActiveRange: Double = sqrt(3)
// }

// object Linear extends ActivationFunction {
// 	def apply(x: Double) = x
// 	def gradient(x: Double) = 1.0
// 	override val lowerActiveRange = Double.MinValue
// 	override val upperActiveRange = Double.MaxValue
// }

// object Sigmoid {
// 	def apply() = new Sigmoid(1.0, 1.0)
// 	def apply(l: Double, g: Double, o: Double) = new Sigmoid(l, g, o)
// }

// class Sigmoid(
// 	val lambda: Double = 1.0,
// 	val gamma: Double = 1.0,
// 	val offset: Double = 0.0)
// 		extends ActivationFunction {

// 	def apply(x: Double) = gamma / (1.0 + exp(-1.0 * lambda * (x - offset)))
// 	def gradient(x: Double) = x * (1 - x)
// }

// object TanH extends ActivationFunction {
// 	def apply(x: Double) = tanh(x)
// 	def gradient(x: Double) = 1 - tanh(x) ** 2
// }

