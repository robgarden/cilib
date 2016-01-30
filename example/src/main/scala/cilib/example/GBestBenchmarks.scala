package cilib
package example

import cilib.Defaults.gbest

import scalaz.effect._
import scalaz.effect.IO.putStrLn
import spire.implicits._

import scalaz.Scalaz._

object GBestBenchmarks extends SafeApp {

  // Define a normal GBest PSO and run it for a single iteration
  val cognitive = Guide.pbest[Mem[List,Double],List,Double]
  val social = Guide.gbest[Mem[List,Double],List,Double]


  for (p <- Problems.benchmarkSet(2)) {
    // RVar
    val bounds = Interval(closed(p.l),closed(p.u))^p.dim

    val gbestPSO = Defaults.gbestBounded(0.729844, 1.496180, 1.496180, cognitive, social, bounds.list)

    val swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(bounds, 20)
    val syncGBest = Iteration.sync(gbestPSO)

    val finalParticles = Runner.repeat(1000, syncGBest, swarm).run(Min)(p.problem).eval(RNG.fromTime)
    val bestParticle = finalParticles.map(_.state.b).reduce((a, b) => Fitness.compare(a, b) run Min)
    val fitnesses = finalParticles.traverse(e => e.state.b.fit).map(_.map(_.fold(_.v,_.v)))

    val best = fitnesses.map(_.min).getOrElse(-666)
    println(s"${p.name} \t\t\t ${bestParticle}")
  }

  // Our IO[Unit] that runs at the end of the world
  override val runc: IO[Unit] =
    putStrLn("Done...")
}
