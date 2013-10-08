package net.sourceforge.cilib.measurement.functionmetric

/**
 * Fitness Distance Correlation Searchability
 *
 * [−1, 1]: For a minimisation problem, 1 indicates the highest measure of
 * searchability (perfect correlation between fitness values and distance to
 * the fittest solution).
 */
class FDCSearchability extends ScalaFunctionMetric {
  def apply(points: List[Point]): Double = {
    val xstar = points sortBy { _.fitness } head

    val fitnesses = points map { _.fitness }
    val fbar = fitnesses.sum.toDouble / fitnesses.size

    val distances = points map { xi => Euclidean(xi, xstar) }
    val dbar = distances.sum.toDouble / fitnesses.size

    val fiMinusFBar = fitnesses map { _ - fbar }
    val diMinusDBar = distances map { _ - dbar }

    val numer = fiMinusFBar.zip(diMinusDBar).map { case (a, b) => a * b } sum
    val denom: Double = math.sqrt(fiMinusFBar.map(fi => fi * fi).sum) *
      math.sqrt(diMinusDBar.map(di => di * di).sum)

    numer / denom
  }
}
