package cilib
package example

import cilib.Defaults.gbest

import scalaz.effect._
import scalaz.effect.IO.putStrLn
import scalaz.std.list._
import spire.implicits._

case class ProblemDef(name: String, problem: Eval[List,Double], l: Double, u: Double, dim: Int)

object GBestPSO extends SafeApp {
  import java.io._

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) = {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  val problems = List(
    ProblemDef("spherical", Problems.spherical, -100.0, 100.0, 10),
    ProblemDef("absolute", Problems.absolute, -100.0, 100.0, 10),
    ProblemDef("hyperEllipsoid", Problems.hyperEllipsoid, -10.0, 10.0, 10),
    ProblemDef("schwefel221", Problems.schwefel221, -500.0, 500.0, 10)
  )

  // Define a normal GBest PSO and run it for a single iteration
  val cognitive = Guide.pbest[Mem[List,Double],List,Double]
  val social = Guide.gbest[Mem[List,Double],List]

  val p1 = List(0.1, 0.3, 0.5, 0.7, 0.9)
  val p2 = List(0.3, 0.7, 1.1, 1.5, 1.9)
  val p3 = List(0.3, 0.7, 1.1, 1.5, 1.9)

  val repeats = 1
  val iterations = 1000

  import scalaz._
  import Scalaz._

  problems.foreach { prob =>

    val averages = for {

      p1i <- p1
      p2i <- p2
      p3i <- p3

      //val gbestPSO = gbest(0.729844, 1.496180, 1.496180, cognitive, social)
      gbestPSO = gbest(p1i, p2i, p3i, cognitive, social)

      repeat = (for {
        i <- 0 until repeats
        // RVar
        swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(Interval(closed(prob.l),closed(prob.u))^prob.dim, 20)
        syncGBest = Iteration.sync(gbestPSO)

        finalParticles = Runner.repeat(iterations, syncGBest, swarm).run(Min)(prob.problem).run(RNG init 1)._2
        fitnesses = finalParticles.traverse(e => e.state.b.fit).map(_.map(_.fold(_.v,_.v)))
        best = fitnesses.map(_.min)
      } yield best).toList.traverse(x => x)

      avg = repeat.map(l => l.sum / l.length).getOrElse(-666.0)

    } yield s"${prob.name},$p1i,$p2i,$p3i,$avg"

//    printToFile(new File(s"/Users/robertgarden/Desktop/gbestpso_${prob.name}.txt")) { p =>
//      averages.foreach(p.println)
//    }

    println(s"${prob.name} done")
  }

//  val best = bests.reduceLeft((a, b) => if (a._3 < b._3) a else b)

  //val lines = bests.mkString("\n")

  // Our IO[Unit] that runs at the end of the world
  override val runc: IO[Unit] =
    putStrLn("Done")
}
