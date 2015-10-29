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

  val dim = 10

  val problemsClasses = Map(
    "um-s" -> List(
      ProblemDef("absolute", Problems.absolute, -100.0, 100.0, dim),
      ProblemDef("differentPowers", Problems.differentPowers, -100.0, 100.0, dim),
      ProblemDef("hyperEllipsoid", Problems.hyperEllipsoid, -10.0, 10.0, dim),
      ProblemDef("powellSum", Problems.powellSum, -1.0, 1.0, dim),
      ProblemDef("spherical", Problems.spherical, -100.0, 100.0, dim)
    ),
    "um-ns" -> List(
      ProblemDef("brent", Problems.brent, -10.0, 10.0, dim),
      ProblemDef("dixonPrice", Problems.dixonPrice, -10.0, 10.0, dim),
      ProblemDef("katsuura", Problems.katsuura, 0.0, 100.0, dim),
      ProblemDef("quadric", Problems.quadric, -100.0, 100.0, dim),
      ProblemDef("zakharov", Problems.zakharov, -5.0, 10.0, dim)
    ),
    "mm-s" -> List(
      ProblemDef("alpine1", Problems.alpine1, -10.0, 10.0, dim),
      ProblemDef("michalewicz", Problems.michalewicz, 0.0, Math.PI, dim),
      ProblemDef("step1", Problems.step1, -100.0, 100.0, dim),
      ProblemDef("vincent", Problems.vincent, 0.25, 10.0, dim),
      ProblemDef("weierstrass", Problems.weierstrass, -0.5, 0.5, dim)
    ),
    "mm-ns" -> List(
      ProblemDef("ackley", Problems.ackley, -32.768, 32.768, dim),
      ProblemDef("eggHolder", Problems.eggHolder, -512.0, 512.0, dim),
      ProblemDef("exponential1", Problems.exponential1, -1.0, 1.0, dim),
      ProblemDef("norwegian", Problems.norwegian, -1.1, 1.1, dim),
      ProblemDef("salomon", Problems.salomon, -100.0, 100.0, dim)
    )
  )

  // Define a normal GBest PSO and run it for a single iteration
  val cognitive = Guide.pbest[Mem[List,Double],List,Double]
  val social = Guide.gbest[Mem[List,Double],List]

  val p1 = List(0.1, 0.3, 0.5, 0.7, 0.9)
  val p2 = List(0.3, 0.7, 1.1, 1.5, 1.9)
  val p3 = List(0.3, 0.7, 1.1, 1.5, 1.9)

  val repeats = 30
  val iterations = 1000

  import scalaz._
  import Scalaz._

  problemsClasses.foreach { case (probClass, problems) =>

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

      } yield s"${probClass},${prob.name},$p1i,$p2i,$p3i,$avg"

     printToFile(new File(s"/Users/robgarden/Desktop/results/gbestpso_${probClass}_${prob.name}.txt")) { p =>
       averages.foreach(p.println)
     }

      println(s"${prob.name} done")
    }

    println("===============================")
    println(s"Problem class: $probClass done")
  }

//  val best = bests.reduceLeft((a, b) => if (a._3 < b._3) a else b)

  //val lines = bests.mkString("\n")

  // Our IO[Unit] that runs at the end of the world
  override val runc: IO[Unit] =
    putStrLn("Done")
}
