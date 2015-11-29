package cilib
package example

import cilib.Defaults.gbest

import scalaz._
import Scalaz._
import effect._
import effect.IO.putStrLn

import spire.implicits._
import pl.project13.scala.rainbow._

import scalaz.concurrent.Task

object HypercubePSO extends SafeApp {
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

  val output = "/home/robertgarden/Dropbox/results/hypercube"

  val strat = "hypercube"

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

  def hyperTask(w: Double, c1: Double, c2: Double, prob: ProblemDef, seed: Long): Task[Maybe[Double]] = Task {
      val cognitive = Guide.pbest[Mem[List,Double],List,Double]
      val hypercube = Selection.hypercube[Particle[Mem[List,Double],List, Double]]
      val (strategy, guide) = ("hypercube", Guide.nbest(hypercube))

      val hyperPSO = gbest(w, c1, c2, cognitive, guide)

      val swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(Interval(closed(prob.l),closed(prob.u))^prob.dim, 32)
      val syncGBest = Iteration.sync(hyperPSO)
      val finalParticles = Runner.repeat(iterations, syncGBest, swarm).run(Min)(prob.problem).eval(RNG init seed)
      val fitnesses = finalParticles.traverse(e => e.state.b.fit).map(_.map(_.fold(_.v,_.v)))

      val percent = params.indexOf((w,c1,c2)).toDouble / params.length * 100
      println(f"${prob.name} $seed $percent%2.2f" + "%")
      fitnesses.map(_.min)
  }

  def hyperTaskLine(w: Double, c1: Double, c2: Double, probClass: String, prob: ProblemDef): Task[String] = {
    val tasks: Task[List[Maybe[Double]]] = Task.gatherUnordered((0 until repeats).toList.map(i => hyperTask(w, c1, c2, prob, i.toLong)))
    val tasksAvg: Task[Double] = tasks.map(lf => lf.sequence.map(l => l.sum / l.length).getOrElse(-666))
    tasksAvg.map(avg => s"$strat,$iterations,$repeats,$probClass,${prob.name},$dim,$w,$c1,$c2,$avg")
  }

  def problemTasks(probClass: String, prob: ProblemDef): Task[List[String]] = {
    val futureLines: List[Task[String]] = for {
      (w, c1, c2) <- params
    } yield hyperTaskLine(w, c1, c2, probClass, prob)

    Task.gatherUnordered(futureLines)
  }

  def writeOut(s: String, probClass: String, name: String, lines: List[String]) = {
      printToFile(new File(s"$output/${s}_${probClass}_${name}.txt")) { p =>
        p.println(s"Strategy,Iterations,Repeat,ProblemClass,Problem,Dimension,w,c1,c2,avgbest")
        lines.foreach(p.println)
      }
  }

  problemsClasses.foreach { case (probClass, problems) =>
    val probsTasks: List[(ProblemDef, Task[List[String]])] = problems.map(p => (p, problemTasks(probClass, p)))
    probsTasks.foreach {
      case (p, t) => {
        writeOut(strat, probClass, p.name, t.run)
      }
    }
  }

  // Our IO[Unit] that runs at the end of the world
  override val runc: IO[Unit] = putStrLn("Done")
}
