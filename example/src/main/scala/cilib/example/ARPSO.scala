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

object ARPSO extends SafeApp {
  import java.io._

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) = {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  val dim = 10
  val problemsClasses = Problems.problemsClasses(dim)

  val params = for {
    l <- List(5.0, 10.0, 20.0, 25.0, 50.0)
    h <- List(55.0, 70.0, 100.0, 200.0, 500.0)
  } yield (l, h)

  val repeats = 30
  val iterations = 1000

  val output = "/home/robertgarden/Dropbox/results/arpso"

  val strat = "arpso"

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

  def arpsoFuture(l: Double, h: Double, prob: ProblemDef, seed: Long): Future[Maybe[Double]] = Future {
    val length = math.abs(prob.u - prob.l)
    val domain = Interval(closed(prob.l),closed(prob.u))^prob.dim

    val cognitive = Guide.pbest[Mem[List,Double],List,Double]
    val social = Guide.gbest[Mem[List,Double],List,Double]

    val gbestPSO = Defaults.arpso(0.729844, 1.496180, 1.496180, length, l, h, cognitive, social, domain.list)

  def pso: List[Particle[Mem[List,Double],List,Double]] => Particle[Mem[List,Double],List,Double] => StepS[List,Double,PSO.ARPSOParams,Result[Particle[Mem[List,Double],List,Double]]] = xs => x => StepS(gbestPSO(xs)(x))

    val swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(domain, 20)
    val syncGBest = Iteration.syncS(pso)
    val finalParticles = Runner.repeatS(iterations, syncGBest, swarm).run(PSO.defaultARPSOParams).run(Min)(prob.problem).eval(RNG init seed)._2
    val fitnesses = finalParticles.traverse(e => e.state.b.fit).map(_.map(_.fold(_.v,_.v)))

    val percent = params.indexOf((l, h)).toDouble / params.length * 100
    println(f"${prob.name} $seed $percent%2.2f" + "%")
    fitnesses.map(_.min)
  }

  def arpsoFutureLine(l: Double , h: Double, probClass: String, prob: ProblemDef): Future[String] = {
    val futures: Future[List[Maybe[Double]]] = Future.sequence((0 until repeats).toList.map(i => arpsoFuture(l, h, prob, i.toLong)))
    val futuresAvg: Future[Double] = futures.map(lf => lf.sequence.map(l => l.sum / l.length).getOrElse(-666))
    futuresAvg.map(avg => s"$strat,$iterations,$repeats,$probClass,${prob.name},$dim,$l,$h,$avg")
  }

  def problemFuture(probClass: String, prob: ProblemDef): Future[List[String]] = {
    val futureLines: List[Future[String]] = for {
      (l, h) <- params
    } yield arpsoFutureLine(l, h, probClass, prob)

    Future.sequence(futureLines)
  }

  def writeOut(s: String, probClass: String, name: String, lines: List[String]) = {
      printToFile(new File(s"$output/${s}_${probClass}_${name}.txt")) { p =>
        p.println(s"Strategy,Iterations,Repeat,ProblemClass,Problem,Dimension,l,h,avgbest")
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
