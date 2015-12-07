// package cilib
// package example

// import cilib.Defaults.constrictionPSO

// import scalaz.effect._
// import scalaz.effect.IO.putStrLn
// import scalaz.std.list._
// import spire.implicits._

// object FDRPSO extends SafeApp {

//   val sum = Problems.spherical

//   // Define a normal GBest PSO and run it for a single iteration
//   val cognitive = (Guide.pbest[Mem[List,Double],List,Double], 1.0)
//   val social = (Guide.gbest[Mem[List,Double],List], 1.0)
//   val fdr = (Guide.fdr[Mem[List,Double]], 1.0)

//   val fdrPSO = constrictionPSO(0.729844, List(cognitive, social, fdr))

//   // RVar
//   val swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(Interval(closed(-5.12),closed(5.12))^10, 20)
//   val syncGBest = Iteration.sync(fdrPSO)

//   // Our IO[Unit] that runs at the end of the world
//   override val runc: IO[Unit] =
//     putStrLn(Runner.repeat(1000, syncGBest, swarm).run(Min)(sum).run(RNG.fromTime).toString)
// }
package cilib
package example

import cilib.Defaults.constrictionPSO

import scalaz._
import Scalaz._
import effect._
import effect.IO.putStrLn

import scalaz.concurrent.Task

import spire.implicits._
import pl.project13.scala.rainbow._

object FDRPSO extends SafeApp {
  import java.io._

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) = {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  val dim = 10
  val problemsClasses = Problems.problemsClasses(dim)

  val params = for {
    w  <- List(0.1, 0.3, 0.5, 0.7, 0.9)
    c1 <- List(0.3, 0.7, 1.1, 1.5, 1.9)
    c2 <- List(0.3, 0.7, 1.1, 1.5, 1.9)
  } yield (w, c1, c2)

  val repeats = 30
  val iterations = 1000

  val output = "/home/robertgarden/Dropbox/fdr1"

  val strat = "fdr"

  println()
  println(s"Running: '${strat.magenta}':")
  println(s"Iterations: \t $iterations")
  println(s"Repeats: \t $repeats")
  println(s"Problems: \t ${problemsClasses.values.flatten.size}")
  println(s"Dimension: \t $dim")
  println(s"Combinations: \t ${params.size}")
  println("Total Runs: \t " + s"${repeats * params.size * problemsClasses.values.flatten.size}".red)
  println(s"Writing To: \t ${output.green}")
  println("Starting...".yellow)
  println()

  problemsClasses.foreach { case (probClass, problems) =>

    println(s"Problem class: ${probClass.yellow}")
    println("========================")

    problems.foreach { prob =>

      print(s"${prob.name.yellow}")

      val averages = for {
        (w, c1, c2) <- params

        cognitive = (Guide.pbest[Mem[List,Double],List,Double], c1)
        fdr       = (Guide.fdr[Mem[List,Double]], c2)

        fdrPSO = constrictionPSO(w, List(cognitive, fdr))

        bests = for {
          i <- 0 until repeats

          swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(Interval(closed(prob.l),closed(prob.u))^prob.dim, 20)
          syncGBest = Iteration.sync(fdrPSO)

          finalParticles = Runner.repeat(iterations, syncGBest, swarm).run(Min)(prob.problem).eval(RNG init i.toLong)
          fitnesses = finalParticles.traverse(e => e.state.b.fit).map(_.map(_.fold(_.v,_.v)))
          best = fitnesses.map(_.min)

        } yield best

        index = params.indexOf((w, c1, c2))
        myprint = {
          print("\r")
          print(s"${prob.name.yellow} ")
          val percent = (index.toDouble + 1.0) / params.size * 100
          print(f"$percent%2.2f" + "%")
        }

        avg = bests.toList.traverse(x => x).map(l => l.sum / l.length).getOrElse(-666.0)

      } yield s"$strat,$iterations,$repeats,$probClass,${prob.name},$dim,$w,$c1,$c2,$avg"

      println()

      printToFile(new File(s"$output/${strat}_${probClass}_${prob.name}.txt")) { p =>

        p.println(s"$strat,Iterations,Repeat,ProblemClass,Problem,Dimension,w,c1,c2,avgbest")
        averages.foreach(p.println)

      }

    }

    println()
    println("Done".green)
    println()
  }

  // Our IO[Unit] that runs at the end of the world
  override val runc: IO[Unit] =
    putStrLn("")
}
