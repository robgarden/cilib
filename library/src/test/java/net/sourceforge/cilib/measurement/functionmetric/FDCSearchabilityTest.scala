package net.sourceforge.cilib.measurement.functionmetric

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class FDCSearchabilitySpec extends FlatSpec with ShouldMatchers {
	"FDCSearchability" should "be in the range [-1,1]" in {
		val points = List(
			Point(Array(0, 1), 4.0),
			Point(Array(1, 2), 1.0),
			Point(Array(2, 4), -6.0)
		)

		val value = new FDCSearchability().apply(points)
		value should be (0.0 plusOrMinus 1.0)
	}
}