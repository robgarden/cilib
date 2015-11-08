// package cilib
// package example

// import cilib.Defaults.constrictionPSO

// import scalaz._
// import Scalaz._
// import effect._
// import effect.IO.putStrLn

// import spire.math._
// import spire.implicits._

// object FERPSO extends SafeApp {

//   val sum = Problems.spherical
//   val dim = 30
//   val bounds = cilib.Interval(closed(-10.0), closed(10.0))
//   val searchSpace = bounds^dim

//   // scaling factor s for FER (diagonal size of search space)
//   val s = sqrt(searchSpace.map(b => (b.upper.value - b.lower.value) ** 2).foldLeft(0.0)(_ + _))
//   val fer = (Guide.fer[Mem[List,Double],List](s), 1.496180)
//   val cognitive = (Guide.pbest[Mem[List,Double],List,Double], 1.496180)

//   val ferPSO = constrictionPSO(0.729844, List(cognitive, fer))

//   // RVar
//   val swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(searchSpace, 20)
//   val syncGBest = Iteration.sync(ferPSO)

//   // Our IO[Unit] that runs at the end of the world
//   override val runc: IO[Unit] =
//     putStrLn(Runner.repeat(1000, syncGBest, swarm).run(Min)(sum).run(RNG init 1).toString)
// }
package cilib
package example

import cilib.Defaults.constrictionPSO

import scalaz._
import Scalaz._
import effect._
import effect.IO.putStrLn

import spire.implicits._
import pl.project13.scala.rainbow._

object FERPSO extends SafeApp {
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

  val params = for {
    w  <- List(0.1, 0.3, 0.5, 0.7, 0.9)
    c1 <- List(0.3, 0.7, 1.1, 1.5, 1.9)
    c2 <- List(0.3, 0.7, 1.1, 1.5, 1.9)
  } yield (w, c1, c2)

  val repeats = 30
  val iterations = 1000

  val output = "/Users/robertgarden/Desktop/results/fer"

  val strat = "fer"

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

      val s = (1 to prob.dim).toList.map(_ => math.pow(prob.u - prob.l,2)).sum

      val averages = for {
        (w, c1, c2) <- params

        cognitive = (Guide.pbest[Mem[List,Double],List,Double], c1)
        (strategy, guide) = ("fer", (Guide.fer[Mem[List,Double],List](s), c2))

        ferPSO = constrictionPSO(w, List(cognitive, guide))

        bests = for {
          i <- 0 until repeats

          swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(Interval(closed(prob.l),closed(prob.u))^prob.dim, 20)
          syncGBest = Iteration.sync(ferPSO)

          finalParticles = Runner.repeat(iterations, syncGBest, swarm).run(Min)(prob.problem).run(RNG init 1)._2
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

      } yield s"$strategy,$iterations,$repeats,$probClass,${prob.name},$dim,$w,$c1,$c2,$avg"

      println()

      printToFile(new File(s"$output/${strat}_${probClass}_${prob.name}.txt")) { p =>

        p.println(s"Strategy,Iterations,Repeat,ProblemClass,Problem,Dimension,w,c1,c2,avgbest")
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
