package cilib.functions.utilities

import scala.language.implicitConversions
import cilib.functions.continuous.ContinuousFunction
import cilib.functions.continuous.Differentiable
import sys.process.stringSeqToProcess
import scala.language.postfixOps

object implicits {
	implicit def toPlottable(f: ContinuousFunction) = new PlottableFunction(f)
	implicit def toPlottableDiff(f : Differentiable) = new PlottableDifferentiableFunction(f)
}

class PlottableFunction(f: ContinuousFunction) {
	def plot(d: (Int, Int) = (-5, 5)) {
		import java.io._

		val file: Option[File] = Option(new File("/var/tmp/points.txt"))
		val writer: Option[PrintWriter] = file.map(new PrintWriter(_))
		writer.foreach { w =>
			val x = (d._1 * 100 to d._2 * 100).map(_.toDouble / 100)
			for (xi <- x) w.println(xi + " " + f(Vector(xi)))
			w.close

			val gnuplot = "\"set terminal png; set output '/var/tmp/points.png';" +
			"set title ''; plot '/var/tmp/points.txt' w l;\""

			Seq("bash", "-c", "gnuplot -e " + gnuplot)!!

			Seq("open", "/var/tmp/points.png")!!
		}
	}
}

class PlottableDifferentiableFunction(f: Differentiable) {
	def plotG(d: (Int, Int) = (-5, 5)) {
		import java.io._

		val file: Option[File] = Option(new File("/var/tmp/pointsG.txt"))
		val writer: Option[PrintWriter] = file.map(new PrintWriter(_))
		writer.foreach { w =>
			val x = (d._1 * 100 to d._2 * 100).map(_.toDouble / 100)
			for (xi <- x) w.println(xi + " " + f.gradient(Vector(xi)).head)
			w.close

			val gnuplot = "\"set terminal png; set output '/var/tmp/pointsG.png';" +
			"set title ''; plot '/var/tmp/pointsG.txt' w l lt rgb 'blue';\""

			Seq("bash", "-c", "gnuplot -e " + gnuplot)!!

			Seq("open", "/var/tmp/pointsG.png")!!
		}
	}
}