package cilib
package example

import cilib.Defaults.gbest

import scalaz.effect._
import scalaz.effect.IO.putStrLn
import scalaz.std.list._
import spire.implicits._

object MeasureGBest extends SafeApp {

  val sum = Problems.spherical

  val cognitive = Guide.pbest[Mem[List,Double],List,Double]; val social = Guide.gbest[Mem[List,Double],List]

  val gbestPSO = gbest(0.729844, 1.496180, 1.496180, cognitive, social)

  val swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(Interval(closed(-5.12),closed(5.12))^30,20)
  val syncGBest = Iteration.sync(gbestPSO)

  def fitness(n: Int, coll: List[Particle[Mem[List,Double],List,Double]])(implicit M: Memory[Mem[List,Double],List,Double]) = {
    val fittest = coll.map(e => M._memory.get(e.state))
        .reduceLeft((a, c) => Fitness.compare(a, c) run Min)
        .fit.map(_.fold(_.v,_.v))

    (n, fittest)
  }

  override val runc: IO[Unit] =
    putStrLn(Runner.repeatWithMeasurement(1000, syncGBest, swarm, fitness).run(Min)(sum).eval(RNG.fromTime).toString)
}
