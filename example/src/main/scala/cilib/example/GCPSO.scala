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

object GCPSO extends SafeApp {
  import java.io._

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) = {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  val dim = 10
  val problemsClasses = Problems.problemsClasses(dim)

  val params = for {
    p <- List(0.5, 1.0, 2.0, 5.0, 10.0)
    s <- List(1, 5, 10, 15, 25)
    f <- List(1, 5, 10, 15, 25)
  } yield (p, s, f)

  val repeats = 30
  val iterations = 1000

  val output = "/home/robertgarden/Dropbox/results/gcpso"

  val strat = "gcpso"

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

  def gcpsoFuture(p: Double, s: Int, f: Int, prob: ProblemDef, seed: Long): Future[Maybe[Double]] = Future {
    val domain = Interval(closed(prob.l),closed(prob.u))^prob.dim
    val cognitive = Guide.pbest[Mem[List,Double],List,Double]
    val gcparams = PSO.GCParams(p, 0, 0, s, f)

    val gbestPSO = Defaults.gcpso(0.729844, 1.496180, 1.496180, cognitive, domain.list)

  def pso: List[Particle[Mem[List,Double],List,Double]] => Particle[Mem[List,Double],List,Double] => StepS[List,Double,PSO.GCParams,Result[Particle[Mem[List,Double],List,Double]]] = xs => x => StepS(gbestPSO(xs)(x))

    val swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(domain, 20)
    val syncGBest = Iteration.syncS(pso)
    val finalParticles = Runner.repeatS(iterations, syncGBest, swarm).run(gcparams).run(Min)(prob.problem).eval(RNG init seed)._2
    val fitnesses = finalParticles.traverse(e => e.state.b.fit).map(_.map(_.fold(_.v,_.v)))

    val percent = params.indexOf((p, s, f)).toDouble / params.length * 100
    println(f"${prob.name} $seed $percent%2.2f" + "%")
    fitnesses.map(_.min)
  }

  def gcpsoFutureLine(p: Double , s: Int, f: Int, probClass: String, prob: ProblemDef): Future[String] = {
    val futures: Future[List[Maybe[Double]]] = Future.sequence((0 until repeats).toList.map(i => gcpsoFuture(p, s, f, prob, i.toLong)))
    val futuresAvg: Future[Double] = futures.map(lf => lf.sequence.map(l => l.sum / l.length).getOrElse(-666))
    futuresAvg.map(avg => s"$strat,$iterations,$repeats,$probClass,${prob.name},$dim,$p,$s,$f,$avg")
  }

  def problemFuture(probClass: String, prob: ProblemDef): Future[List[String]] = {
    val futureLines: List[Future[String]] = for {
      (p, s, f) <- params
    } yield gcpsoFutureLine(p, s, f, probClass, prob)

    Future.sequence(futureLines)
  }

  def writeOut(s: String, probClass: String, name: String, lines: List[String]) = {
      printToFile(new File(s"$output/${s}_${probClass}_${name}.txt")) { p =>
        p.println(s"Strategy,Iterations,Repeat,ProblemClass,Problem,Dimension,p,s,f,avgbest")
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
