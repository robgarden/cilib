package cilib
package example

import cilib.Defaults.constrictionPSO

import scalaz._
import Scalaz._
import effect._
import effect.IO.putStrLn

import spire.implicits._
import pl.project13.scala.rainbow._

import scala.concurrent._
import ExecutionContext.Implicits.global

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

  val output = "/Users/robertgarden/Desktop/fer"

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

  def ferFuture(w: Double, c1: Double, c2: Double, prob: ProblemDef, seed: Long): Future[Maybe[Double]] = Future {
      val s = (1 to prob.dim).toList.map(_ => math.pow(prob.u - prob.l,2)).sum
      val cognitive = (Guide.pbest[Mem[List,Double],List,Double], c1)
      val guide = (Guide.fer[Mem[List,Double],List](s), c2)

      val ferPSO = constrictionPSO(w, List(cognitive, guide))

      val swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(Interval(closed(prob.l),closed(prob.u))^prob.dim, 20)
      val syncGBest = Iteration.sync(ferPSO)
      val finalParticles = Runner.repeat(iterations, syncGBest, swarm).run(Min)(prob.problem).eval(RNG init seed)
      val fitnesses = finalParticles.traverse(e => e.state.b.fit).map(_.map(_.fold(_.v,_.v)))

      println(s"Seed: $seed")

      fitnesses.map(_.min)
  }


  def ferFutureLine(w: Double, c1: Double, c2: Double, probClass: String, prob: ProblemDef): Future[String] = {
    val futures: Future[List[Maybe[Double]]] = Future.sequence((0 until repeats).toList.map(i => ferFuture(w, c1, c2, prob, i.toLong)))
    val futuresAvg: Future[Double] = futures.map(lf => lf.sequence.map(l => l.sum / l.length).getOrElse(-666))
    futuresAvg.map(avg => s"$strat,$iterations,$repeats,$probClass,${prob.name},$dim,$w,$c1,$c2,$avg")
  }

  def problemFuture(probClass: String, prob: ProblemDef): Future[List[String]] = {
    val futureLines: List[Future[String]] = for {
      (w, c1, c2) <- params
    } yield ferFutureLine(w, c1, c2, probClass, prob)

    println()
    println(s"Done ${prob.name}")
    println()

    Future.sequence(futureLines)
  }

  def writeOut(s: String, probClass: String, name: String, lines: List[String]) = {
      printToFile(new File(s"$output/${s}_${probClass}_${name}.txt")) { p =>
        p.println(s"Strategy,Iterations,Repeat,ProblemClass,Problem,Dimension,w,c1,c2,avgbest")
        lines.foreach(p.println)
      }
  }

  problemsClasses.foreach { case (probClass, problems) =>
    val probsFutures: List[(ProblemDef, Future[List[String]])] =  problems.map(p => (p, problemFuture(probClass, p)))
    probsFutures.foreach {
      case (p, f) => f onSuccess { case ls => writeOut(strat, probClass, p.name, ls) }
    }
  }

  // Our IO[Unit] that runs at the end of the world
  override val runc: IO[Unit] = putStrLn("Done")
}
