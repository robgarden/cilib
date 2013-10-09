package net.sourceforge.cilib.measurement.functionmetric

import fj.F;
import net.sourceforge.cilib.algorithm.AbstractAlgorithm;
import net.sourceforge.cilib.problem.FunctionOptimisationProblem
import net.sourceforge.cilib.controlparameter.DomainProportionalControlParameter
import net.sourceforge.cilib.math.random.GaussianDistribution
import net.sourceforge.cilib.`type`.types.container.Vector
import net.sourceforge.cilib.math.random.generator.Rand
import net.sourceforge.cilib.`type`.types.Bounds
import net.sourceforge.cilib.`type`.types.Numeric
import net.sourceforge.cilib.`type`.types.container.StructuredType;

/**
 * Fitness Cloud Index
 *
 * [0, 1]: indicating the proportion of fitness improving solutions after two
 * PSO updates.
 */
class FitnessCloudIndex extends ScalaFunctionMetric {
  // pso parameters
  val inertia = 0.729844
  val c1 = 1.496180
  val c2 = 1.496180

  // type 1 = cog
  // tyoe 2 = soc
  var updateType = 1
  var hasSetup = false

  val domainRangeParameter = new DomainProportionalControlParameter()
  var scalaFunc: ScalaFunctionWrapper = null
  var buildRep: StructuredType[Numeric] = Vector.of(0.0)
  var ranges: Array[Bounds] = Array()
  var gaussian: GaussianDistribution = new GaussianDistribution()

  def setup {
    // get 10% of domain range
    domainRangeParameter.setProportion(0.1)

    // get domain ranges
    buildRep = AbstractAlgorithm.get().getOptimisationProblem()
      .getDomain().getBuiltRepresentation() match {
        case a: StructuredType[Numeric] => a
        case _ => throw new UnsupportedOperationException
      }

    ranges = Array.fill(buildRep.size)(new Bounds(0, 0))
    var i = 0
    val iter: java.util.Iterator[Numeric] = buildRep.iterator
    while (iter.hasNext) {
      val n = iter.next
      ranges(i) = n.getBounds()
      i = i + 1
    }

    // set gaussian distribution deviation to 10% domain range
    gaussian.setDeviation(domainRangeParameter)

    // get function
    val problem = AbstractAlgorithm.get().getOptimisationProblem
    val func = problem match {
      case fop: FunctionOptimisationProblem => fop.getFunction() match {
        case dfunc: F[Vector, Double] => dfunc
        case _ => throw new UnsupportedOperationException
      }
      case _ => throw new UnsupportedOperationException
    }

    scalaFunc = new ScalaFunctionWrapper(func)
  }

  def generatePBests(particles: List[Particle]): List[Particle] = {
    particles map { p =>
      val zi = p.curr.x.zip(ranges).map {
        case (xi: Double, b: Bounds) => {
          var zi = xi + gaussian.getRandomNumber
          while (!b.isInsideBounds(zi)) {
            zi = xi + gaussian.getRandomNumber
          }
          zi
        }
      }

      val z = new Point(zi, scalaFunc(zi))

      if (z.fitness < p.curr.fitness) Particle(p.curr, z)
      else Particle(z, p.curr)
    }
  }

  def updatePosition(particles: List[Particle]): List[Particle] = {
    val updated = particles map { p =>
      val newPosition = p.curr.x.zip(p.velocity).map {
        case (x, v) =>
          x + v
      }
      val newPoint = Point(newPosition, scalaFunc(newPosition))
      Particle(newPoint, p.pbest, p.velocity)
    }

    repair(updated)
  }

  def updateVelocity(particles: List[Particle]): List[Particle] = {
    if (updateType == 1) {
      // cognitive update
      particles map cognitiveUpdate
    } else {
      // social update
      val gbest = particles.sortBy(_.pbest.fitness).head
      particles map { p => socialUpdate(p, gbest) }
    }
  }

  def cognitiveUpdate(particle: Particle): Particle = {
    val velocity = (for {
      i <- 0 until particle.velocity.size
      velTerm = particle.velocity(i) * inertia
      socTerm = c1 * Rand.nextDouble * (particle.pbest.x(i) - particle.curr.x(i))
    } yield velTerm + socTerm).toArray

    Particle(particle.curr, particle.pbest, velocity)
  }

  def socialUpdate(particle: Particle, gbest: Particle): Particle = {
    val velocity = (for {
      i <- 0 until particle.velocity.size
      velTerm = particle.velocity(i) * inertia
      socTerm = c2 * Rand.nextDouble * (gbest.pbest.x(i) - particle.curr.x(i))
    } yield velTerm + socTerm).toArray

    Particle(particle.curr, particle.pbest, velocity)
  }

  def updatePBest(particles: List[Particle]): List[Particle] = {
    particles map { p =>
      if (p.curr.fitness < p.pbest.fitness)
        Particle(p.curr, p.curr, p.velocity)
      else
        p
    }
  }

  def repair(particles: List[Particle]): List[Particle] = {
    particles map { p =>
      val repairedCurrX = p.curr.x.zip(ranges) map {
        case (xi: Double, b: Bounds) =>
          if (b.isInsideBounds(xi)) {
            xi
          } else {
            if (xi < b.getLowerBound) b.getLowerBound()
            else b.getUpperBound()
          }
      }
      val repairedCurr = Point(repairedCurrX, scalaFunc(repairedCurrX))
      Particle(repairedCurr, p.pbest, p.velocity)
    }
  }

  def setUpdateType(x: Int) = updateType = x

  def apply(points: List[Point]): Double = {
    if (!hasSetup) {
      setup
      hasSetup = true
    }

    val p0 = generatePBests(points map (Particle(_)))
    val p1 = updatePBest(updatePosition(updateVelocity(p0)))
    val p2 = updatePosition(updateVelocity(p1))

    val p0Fitness = p0.map(_.curr.fitness)
    val p2Fitness = p2.map(_.curr.fitness)

    val fitnesses = p0Fitness ++ p2Fitness
    // we switch them around here
    val max: Double = fitnesses.min
    val min: Double = fitnesses.max

    val results: List[(Double, Double)] = p0Fitness.zip(p2Fitness).map {
      case (o: Double, n: Double) => {
        val on = normalise(o, min, max)
        val nn = normalise(n, min, max)
        // println(s"${on}, ${nn}")
        (on, nn)
      }
    }

    results.filter(r => r._2 > r._1).size.toDouble / results.size
  }

  def normalise(x: Double, min: Double, max: Double) = (x - min) / (max - min)
}
