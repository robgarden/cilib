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
 */
class FitnessCloudIndex extends ScalaFunctionMetric {
  // pso parameters
  val inertia = 0.729844
  val c1 = 1.496180
  val c2 = 1.496180

  // type 1 = cog
  // tyoe 2 = soc
  var updateType = 1

  // get 10% of domain range
  val domainRangeParameter = new DomainProportionalControlParameter()
  domainRangeParameter.setProportion(0.1)

  // get domain ranges
  val buildRep: StructuredType[Numeric] = AbstractAlgorithm.get().getOptimisationProblem()
    .getDomain().getBuiltRepresentation() match {
      case a: StructuredType[Numeric] => a
      case _ => throw new UnsupportedOperationException
    }

  val ranges: Array[Bounds] = Array.fill(buildRep.size)(new Bounds(0, 0))
  var i = 0
  val iter: java.util.Iterator[Numeric] = buildRep.iterator
  while (iter.hasNext) {
    val n = iter.next
    ranges(i) = n.getBounds()
    i = i + 1
  }

  // set gaussian distribution deviation to 10% domain range
  val gaussian = new GaussianDistribution()
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
  val scalaFunc = new ScalaFunctionWrapper(func)

  def generatePBests(particles: List[Particle]): List[Particle] = {
    particles map { p =>
      val z = p.curr.x.map { xi =>
        xi + gaussian.getRandomNumber
      }

      val newPoint = new Point(z, scalaFunc(z))
      updatePBest(Particle(newPoint, p.curr))
    }
  }

  def updatePosition(particles: List[Particle]): List[Particle] = {
    particles map { p =>
      val newPosition = p.curr.x.zip(p.velocity).map {
        case (x, v) =>
          x + v
      }
      val newPoint = Point(newPosition, scalaFunc(newPosition))
      Particle(newPoint, p.pbest, p.velocity)
    }
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
      socTerm = c1 * Rand.nextDouble * (gbest.pbest.x(i) - particle.curr.x(i))
    } yield velTerm + socTerm).toArray

    Particle(particle.curr, particle.pbest, velocity)
  }

  def updatePBest(particle: Particle): Particle = {
    if (particle.curr.fitness < particle.pbest.fitness && withinBounds(particle.curr))
      Particle(particle.curr, particle.curr)
    else
      particle
  }

  def withinBounds(point: Point): Boolean = {
    point.x.zip(ranges) forall {
      case (xi: Double, b: Bounds) =>
        b.isInsideBounds(xi)
    }
  }

  def setUpdateType(x: Int) = updateType = x

  def apply(points: List[Point]): Double = {
    val p0 = generatePBests(points map (Particle(_)))
    val yhat0: Double = p0.map(_.pbest.fitness).sorted.head

    val p1 = updatePosition(updateVelocity(p0))
    1.0
  }
}
