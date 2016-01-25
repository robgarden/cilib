package cilib
package example

import cilib.Defaults.gbestBounded

import scalaz._
import Scalaz._
import effect._
import effect.IO.putStrLn

import spire.implicits._
import pl.project13.scala.rainbow._

import scala.concurrent._
import ExecutionContext.Implicits.global

object PCXRepeating extends SafeApp {
  import java.io._

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) = {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  val dim = 10
  val problemsClasses = Problems.problemsClasses(dim)

  val params = for {
    // s1 <- List(1.0, 2.0, 3.0, 5.0, 10.0)
    // s2 <- List(1.0, 2.0, 3.0, 5.0, 10.0)
    s1 <- List(0.01, 0.05, 0.1, 0.2, 0.5)
    s2 <- List(0.01, 0.05, 0.1, 0.2, 0.5)
    rr <- List(5, 10, 20, 50, 100, 500)
  } yield (s1, s2, rr)

  val repeats = 30
  val iterations = 1000

  val output = "/home/robertgarden/Dropbox/results/pcx-repeating"

  val strat = "pcx-repeating"

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

  def pcxFuture(s1: Double, s2: Double, rr: Int, prob: ProblemDef, seed: Long): Future[Maybe[Double]] = Future {
    val cognitive = Guide.pbest[Mem[List,Double],List,Double]

    val s = (1 to prob.dim).toList.map(_ => math.pow(prob.u - prob.l,2)).sum
    val sigma1 = s1 * s
    val sigma2 = s2 * s

    val guide = Guide.repeatingPCX[Mem[List,Double],List](sigma1, sigma2, rr, (c, _) => c)

    val domain = Interval(closed(prob.l),closed(prob.u))^prob.dim

    // val pcx = gbestBounded(0.729844, 1.496180, 1.496180, cognitive, guide, domain.list)
    val pcx = cilib.Defaults.pcxPSO(guide, domain.list)

    val swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(domain, 20)
    val syncGBest = Iteration.sync(pcx)
    val finalParticles = Runner.repeat(iterations, syncGBest, swarm).run(Min)(prob.problem).eval(RNG init seed)
    val fitnesses = finalParticles.traverse(e => e.state.b.fit).map(_.map(_.fold(_.v,_.v)))

    val percent = params.indexOf((s1,s2,rr)).toDouble / params.length * 100
    println(f"${prob.name} $seed $percent%2.2f" + "%")

    fitnesses.map(_.min)
  }

  def pcxFutureLine(s1: Double, s2: Double, rr: Int, probClass: String, prob: ProblemDef): Future[String] = {
    val futures: Future[List[Maybe[Double]]] = Future.sequence((0 until repeats).toList.map(i => pcxFuture(s1, s2, rr, prob, i.toLong)))
    val futuresAvg: Future[Double] = futures.map(lf => lf.sequence.map(l => l.sum / l.length).getOrElse(-666))
    futuresAvg.map(avg => s"$strat,$iterations,$repeats,$probClass,${prob.name},$dim,$s1,$s2,$rr,$avg")
  }

  def problemFuture(probClass: String, prob: ProblemDef): Future[List[String]] = {
    val futureLines: List[Future[String]] = for {
      (s1, s2, rr) <- params
    } yield pcxFutureLine(s1, s2, rr, probClass, prob)

    Future.sequence(futureLines)
  }

  def writeOut(s: String, probClass: String, name: String, lines: List[String]) = {
      printToFile(new File(s"$output/${s}_${probClass}_${name}.txt")) { p =>
        p.println(s"Strategy,Iterations,Repeat,ProblemClass,Problem,Dimension,s1,s2,rr,avgbest")
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