package cilib
package example

import cilib.Defaults.gbestConstrained
import cilib.Defaults.gbest

import scalaz.effect._
import scalaz.effect.IO.putStrLn
import scalaz.std.list._
import spire.implicits._

object OldGBest extends SafeApp {

  val sum = Problems.vincent

  // Define a normal GBest PSO and run it for a single iteration
  val cognitive = Guide.pbest[Mem[List,Double],List,Double]
  val social = Guide.gbest[Mem[List,Double],List]

  val bounds = Interval(closed(0.25),closed(10.0))^10
  val gbestPSO = gbestConstrained(0.729844, 1.496180, 1.496180, cognitive, social, bounds.list)
  //val gbestPSO = gbest(0.729844, 1.496180, 1.496180, cognitive, social)

  // RVar
  val swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(bounds, 20)
  val syncGBest = Iteration.sync(gbestPSO)

  // Our IO[Unit] that runs at the end of the world
  override val runc: IO[Unit] =
    putStrLn(Runner.repeat(1000, syncGBest, swarm).run(Min)(sum).eval(RNG.fromTime).map(_.state).toString)
}
