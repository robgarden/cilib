package net.sourceforge.cilib.measurement.functionmetric

/**
 * Information Landscape
 *
 * [0, 1]
 */
class InformationLandscape extends ScalaFunctionMetric {
  def apply(points: List[Point]): Double = {

    def ilVector(a: List[Point]): List[Double] = {
      (for {
        i <- 0 until a.length
        i1 <- (i + 1) until a.length
        fi = a(i).fitness
        fi1 = a(i1).fitness
        comp = if (fi < fi1) 1.0 else if (fi == fi1) 0.5 else 0
      } yield comp).toList
    }

    val bestPoint = points.sortBy(x => x.fitness).head
    val pointsIlVector = ilVector(points)

    def sphere(a: List[Point]): List[Point] = {
      (for {
        p <- a
        f = p.x.zip(bestPoint.x).map(xi => math.pow(xi._1 - xi._2, 2)).reduce((xi, yi) => xi + yi)
      } yield Point(p.x, f)).toList
    }

    val spherePoints = sphere(points)
    val spherePointsIlVector = ilVector(spherePoints)

    def distance(a: List[Double], b: List[Double]): Double = {
      a.zip(b).map(z => math.abs(z._1 - z._2)).reduce((x, y) => x + y) / a.length
    }

    distance(pointsIlVector, spherePointsIlVector)
  }
}
