package cilib
package example

import cilib.Defaults.pcxPSO

import scalaz._
import Scalaz._
import effect._
import effect.IO.putStrLn

import spire.implicits._
import pl.project13.scala.rainbow._

import scala.concurrent._
import ExecutionContext.Implicits.global

import cilib.Previous._

object PCXBoltzmannDeb extends SafeApp {
  import java.io._

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) = {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  val dim = 10
  val problemsClasses = Problems.problemsClasses(dim)

  val params = for {
    s1 <- List(0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0)
    s2 <- List(0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0)
  } yield (s1, s2)

  val repeats = 30
  val iterations = 1000

  val output = "/home/robertgarden/Dropbox/results/pcx-boltzmann-deb"

  val strat = "pcx-boltzmann-deb"

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

  def pcxFuture(s1: Double, s2: Double, prob: ProblemDef, seed: Long): Future[Maybe[Double]] = Future {

    val domain = Interval(closed(prob.l),closed(prob.u))^prob.dim

    val temperatures = ParamHelper.decreasing(1, 100, iterations)

    val algs = temperatures.map { temp =>
      val pcx = Defaults.pcxBoltzmanPSODeb(0.729844, 1.496180, 1.496180, s1, s2, temp, domain.list.toList)
      Iteration.sync(pcx)
    }

    val swarm = Position.createCollection(PSO.createParticle(x => Entity(Prev(x, x.zeroed, x), x)))(domain, 20)
    val finalParticles = Runner.repeatA(algs, swarm).run(Min)(prob.problem).eval(RNG init seed)

    val fitnesses = finalParticles.traverse(e => e.state.b.fit).map(_.map(_.fold(_.v,_.v)))

    val percent = params.indexOf((s1,s2)).toDouble / params.length * 100
    println(f"${prob.name} $seed $percent%2.2f" + "%")
    fitnesses.map(_.min)
  }

  def pcxFutureLine(s1: Double, s2: Double, probClass: String, prob: ProblemDef): Future[String] = {
    val futures: Future[List[Maybe[Double]]] = Future.sequence((0 until repeats).toList.map(i => pcxFuture(s1, s2, prob, i.toLong)))
    val futuresAvg: Future[Double] = futures.map(lf => lf.sequence.map(l => l.sum / l.length).getOrElse(-666))
    futuresAvg.map(avg => s"$strat,$iterations,$repeats,$probClass,${prob.name},$dim,$s1,$s2,$avg")
  }

  def problemFuture(probClass: String, prob: ProblemDef): Future[List[String]] = {
    val futureLines: List[Future[String]] = for {
      (s1, s2) <- params
    } yield pcxFutureLine(s1, s2, probClass, prob)

    Future.sequence(futureLines)
  }

  def writeOut(s: String, probClass: String, name: String, lines: List[String]) = {
      printToFile(new File(s"$output/${s}_${probClass}_${name}.txt")) { p =>
        p.println(s"Strategy,Iterations,Repeat,ProblemClass,Problem,Dimension,s1,s2,avgbest")
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
  override val runc: IO[Unit] = putStrLn("Starting...")
}