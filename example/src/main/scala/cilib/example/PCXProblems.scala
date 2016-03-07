package cilib
package example

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

import spire.implicits._

import pl.project13.scala.rainbow._

object PCXProblems {

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) = {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  val dim = 30
  val problems = Problems.benchmarkSet(dim)

  val s1 = 2.582352941
  val s2 = 2.652941176

  val repeats = 30
  val iterations = 1000

  val output = "/home/robertgarden/Dropbox/results/problems"

  val strategy = "pcx"

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

    val guide = Guide.pcx[Mem[List,Double],List](s1, s2)
    val pcx = Defaults.pcxPSO(guide, domain.list.toList)

    val swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(domain, 20)
    val sync = Iteration.sync(pcx)
    val finalParticles = Runner.repeat(iterations, sync, swarm).run(Min)(prob.problem).eval(RNG init seed)
    val fitnesses = finalParticles.traverse(e => e.state.b.fit).map(_.map(_.fold(_.v,_.v)))

    println(s"${prob.name} $seed")

    fitnesses.map(_.min)
  }

  def starLineTask(prob: ProblemDef): Task[String] = {
    val tasks: Task[List[Maybe[Double]]] = Task.gatherUnordered((0 until repeats).toList.map(i => starTask(prob, i.toLong)))
    val taskAvg: Task[String] = tasks.map(l => l.sequence.map(_.mkString(",")).getOrElse("Error"))
    taskAvg.map(avgs => s"$strategy,$iterations,$repeats,${prob.name},$dim,$s1,$s2,$avgs")
  }

  val problemTasks = Task.suspend(Task.gatherUnordered(problems.map(starLineTask)))

  def writeOut(lines: List[String]) = {
    printToFile(new java.io.File(s"${output}/${strategy}.txt")) { p =>
      p.println(s"Strategy,Iterations,Repeat,ProblemClass,Problem,Dimension,s1,s2,avgs")
      lines.foreach(p.println)
    }
  }

  def main(args: Array[String]) = {
    writeOut(problemTasks.unsafePerformSync)
    println("Done")
  }
}
