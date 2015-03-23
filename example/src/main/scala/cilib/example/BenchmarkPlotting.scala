package cilib
package example

import java.io.{File, PrintWriter}

import cilib.Functions._

// import spire.algebra._
import spire.implicits._

object BenchmarkPlotting {

  type BenchmarkFunction = Seq[Double] => Option[Double]

  def plotPoints(f: BenchmarkFunction, bounds: (Int, Int),
    res: Int, output: String) = {

    val file = new File("%s.txt".format(output))
    val w = new PrintWriter(file)

    bounds match {
      case (a, b) => for {
        x <- a to b
        y <- a to b
        z <- f(List(x * 1.0, y * 1.0))
        s = if (y == b) "\n" else ""
        l = "%f %f %f%s".format(x * 1.0, y * 1.0, z, s)
      } w.println(l)
    }

    w.close
  }

  def createGnuplot(output: String) = {
    val file = new File("%s.gnuplot".format(output))
    val w = new PrintWriter(file)
    val lines = List(
      "set terminal png",
      "set output 'plots/%s.png'".format(output),
      "set isosamples 40",
      "splot '%s.txt' with pm3d".format(output)
    )

    lines.foreach(w.println)

    w.close
  }

  def main(args: Array[String]): Unit = {

    import sys.process._

    val outputs = List("spherical", "mishra5", "ackley")
    val benchmarks =
      List(spherical[Double] _, mishra5[Double] _, ackley[Double] _)

      (outputs zip benchmarks).foreach {
        case (o, b) =>
          plotPoints(b, (-50, 50), 10, o)
          createGnuplot(o)

          val commands = List(
            "mkdir -p plots",
            "gnuplot %s.gnuplot".format(o),
            "rm %s.gnuplot".format(o),
            "rm %s.txt".format(o)
          )
          commands.foreach(_ !)
      }
  }

}
