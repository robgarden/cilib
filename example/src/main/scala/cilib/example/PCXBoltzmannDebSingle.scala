package cilib
package example

import scalaz.effect._
import scalaz.effect.IO.putStrLn
import scalaz.std.list._
import spire.implicits._

import cilib.Previous._

object PCXBoltzmannDebSingle extends SafeApp {

  val sum = Problems.sphere

  val domain = Interval(closed(-100.0),closed(100.0))^10

  val temperatures = ParamHelper.decreasing(1, 100, 1000)
  val (s1, s2) = (2.0, 2.0)
  val algs = temperatures.map { temp =>
    val pcx = Defaults.pcxBoltzmanPSODeb(0.729844, 1.496180, 1.496180, s1, s2, temp, domain.list.toList)
    Iteration.sync(pcx)
  }

  val swarm = Position.createCollection(PSO.createParticle(x => Entity(Prev(x, x.zeroed, x), x)))(domain, 20)
  val finalParticles = Runner.repeatA(algs, swarm).run(Min)(sum).eval(RNG.fromTime)

  // Our IO[Unit] that runs at the end of the world
  override val runc: IO[Unit] =
    putStrLn(finalParticles.toString)
}
