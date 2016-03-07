package cilib
package example

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

import spire.implicits._

import pl.project13.scala.rainbow._

object GCPSOProblems {

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) = {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  val dim = 30
  val problems = Problems.benchmarkSet(dim)

  val p = 2.0
  val s = 15
  val f = 25

  val repeats = 30
  val iterations = 1000

  val output = "/home/robertgarden/Dropbox/results/problems"

  val strategy = "gcpso"

  println()
  println(s"Running: '${strategy.magenta}':")
  println(s"Iterations: \t $iterations")
  println(s"Repeats: \t $repeats")
  println(s"Problems: \t ${problems.size}")
  println(s"Dimension: \t $dim")
  println(s"Total Runs: \t ${repeats * problems.size}".red)
  println(s"Writing To: \t ${output.green}")
  println("Starting...".yellow)
  println()

  def starTask(prob: ProblemDef, seed: Long): Task[Maybe[Double]] = Task {
    val domain = Interval(closed(prob.l),closed(prob.u))^prob.dim
    val cognitive = Guide.pbest[Mem[List,Double],List,Double]
    val gcparams = PSO.GCParams(p, 0, 0, s, f)

    val gbestPSO = Defaults.gcpso(0.729844, 1.496180, 1.496180, cognitive, domain.list.toList)

  def pso: List[Particle[Mem[List,Double],List,Double]] => Particle[Mem[List,Double],List,Double] => StepS[List,Double,PSO.GCParams,Result[Particle[Mem[List,Double],List,Double]]] = xs => x => StepS(gbestPSO(xs)(x))

    val swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(domain, 20)
    val syncGBest = Iteration.syncS(pso)
    val finalParticles = Runner.repeatS(iterations, syncGBest, swarm).run(gcparams).run(Min)(prob.problem).eval(RNG init seed)._2
    val fitnesses = finalParticles.traverse(e => e.state.b.fit).map(_.map(_.fold(_.v,_.v)))

    println(s"${prob.name} $seed")

    fitnesses.map(_.min)
  }

  def starLineTask(prob: ProblemDef): Task[String] = {
    val tasks: Task[List[Maybe[Double]]] = Task.gatherUnordered((0 until repeats).toList.map(i => starTask(prob, i.toLong)))
    val taskAvg: Task[String] = tasks.map(l => l.sequence.map(_.mkString(",")).getOrElse("Error"))
    taskAvg.map(avgs => s"$strategy,$iterations,$repeats,${prob.name},$dim,$p,$s,$f,$avgs")
  }

  val problemTasks = Task.suspend(Task.gatherUnordered(problems.map(starLineTask)))

  def writeOut(lines: List[String]) = {
    printToFile(new java.io.File(s"${output}/${strategy}.txt")) { p =>
      p.println(s"Strategy,Iterations,Repeat,ProblemClass,Problem,Dimension,p,s,f,avgs")
      lines.foreach(p.println)
    }
  }

  def main(args: Array[String]) = {
    writeOut(problemTasks.unsafePerformSync)
    println("Done")
  }
}
