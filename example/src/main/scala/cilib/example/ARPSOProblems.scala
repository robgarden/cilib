package cilib
package example

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

import spire.implicits._

import pl.project13.scala.rainbow._

object ARPSOProblems {

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) = {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  val dim = 10
  val problems = Problems.benchmarkSet(dim)
  val repeats = 30
  val iterations = 1000

  val output = "/home/robertgarden/Dropbox/results/problems/"
  // val output = "/Users/robertgarden/Desktop/star"

  val (w, c1, c2) = (0.729844, 1.496180, 1.496180)
  val (l, h) = (13.23529412, 90.0)
  val strategy = "arpso"

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
    val length = math.abs(prob.u - prob.l)
    val domain = Interval(closed(prob.l), closed(prob.u))^prob.dim

    val cognitive = Guide.pbest[Mem[List,Double],List,Double]
    val social = Guide.gbest[Mem[List,Double],List,Double]

    val arpso = Defaults.arpso(w, c1, c2, length, l, h, cognitive, social, domain.list.toList)
    def pso: List[Particle[Mem[List,Double],List,Double]] => Particle[Mem[List,Double],List,Double] => StepS[List,Double,PSO.ARPSOParams,Result[Particle[Mem[List,Double],List,Double]]] = xs => x => StepS(arpso(xs)(x))

    val swarm = Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(domain, 20)
    val sync = Iteration.syncS(pso)
    val finalParticles = Runner.repeatS(iterations, sync, swarm).run(PSO.defaultARPSOParams).run(Min)(prob.problem).eval(RNG init seed)._2
    val fitnesses = finalParticles.traverse(e => e.state.b.fit).map(_.map(_.fold(_.v,_.v)))

    println(s"${prob.name} $seed")

    fitnesses.map(_.min)
  }

  def starLineTask(prob: ProblemDef): Task[String] = {
    val tasks: Task[List[Maybe[Double]]] = Task.gatherUnordered((0 until repeats).toList.map(i => starTask(prob, i.toLong)))
    val taskAvg: Task[String] = tasks.map(l => l.sequence.map(_.mkString(",")).getOrElse("Error"))
    taskAvg.map(avgs => s"$strategy,$iterations,$repeats,${prob.name},$dim,$w,$c1,$c2,$l,$h,$avgs")
  }

  val problemTasks = Task.suspend(Task.gatherUnordered(problems.map(starLineTask)))

  def writeOut(lines: List[String]) = {
    printToFile(new java.io.File(s"${output}/${strategy}.txt")) { p =>
      p.println(s"Strategy,Iterations,Repeat,ProblemClass,Problem,Dimension,w,c1,c2,l,h,avgs")
      lines.foreach(p.println)
    }
  }

  def main(args: Array[String]) = {
    writeOut(problemTasks.unsafePerformSync)
    println("Done")
  }
}
