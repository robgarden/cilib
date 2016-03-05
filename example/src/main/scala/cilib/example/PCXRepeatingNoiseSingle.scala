package cilib
package example

import scalaz.effect._
import scalaz.effect.IO.putStrLn
import scalaz.std.list._
import spire.implicits._

object PCXRepeatingNoiseSingle extends SafeApp {

  val sum = Problems.sphere

  val domain = Interval(closed(-100.0),closed(100.0))^10

  val (s1, s2) = (2.0, 2.0)

  val selection = Selection.star[Entity[Mem[List,Double],List,Double]]
  val pcx = Defaults.pcxPSORepeatingNoise(s1, s2, 5, selection, domain.list.toList)

  val pso = Iteration.sync(pcx)

  val swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(domain, 20)
  val finalParticles = Runner.repeat(1000, pso, swarm).run(Min)(sum).eval(RNG.fromTime)

  // Our IO[Unit] that runs at the end of the world
  override val runc: IO[Unit] =
    putStrLn(finalParticles.toString)
}
