package cilib

import _root_.scala.Predef.{ any2stringadd => _, _ }
import scalaz.NonEmptyList
import scalaz.syntax.traverse._
import scalaz.syntax.apply._
import scalaz.std.list._
import spire.math._
import spire.implicits._

/**
  Examples of how to define "Problem" instances.

  There are two types of problems: dynamic and static. With a static
  problem, the problem space never changes, but in the dynamic environment,
  the problem may change at any time.

  There is a very strong link between the quantification of a problem solution
  and the type of fitness that is the result:
    - Valid(x) -> A valid fitness in the environment with `x` and the value
    - Penalty(x, y) -> A valid fitness, but the solution has had a penalty of `y` applied

  */
case class ProblemDef(name: String, problem: Eval[List,Double], l: Double, u: Double, dim: Int)

object Problems {

  /* Some of the more common static benchmark problems */
  import scalaz._
  import Scalaz._
  import cilib.benchmarks._
  import cilib.Sized._

  // uni-modal / separable
  val absolute  = Unconstrained { (a: List[Double]) => Valid(Benchmarks.absoluteValue(a.convertNel)) }
  val differentPowers = Unconstrained { (a: List[Double]) => Valid(Benchmarks.differentPowers(a.convertSized2And)) }
  val hyperEllipsoid = Unconstrained { (a: List[Double]) => Valid(Benchmarks.hyperEllipsoid(a.convertNel)) }
  val powellSum = Unconstrained { (a: List[Double]) => Valid(Benchmarks.powellSum(a.convertNel)) }
  val spherical = Unconstrained { (a: List[Double]) => Valid(Benchmarks.spherical(a.convertNel)) }

  // uni-modal / non-separable
  val brent = Unconstrained { (a: List[Double]) => Valid(Benchmarks.brent(a.convertNel)) }
  val dixonPrice = Unconstrained { (a: List[Double]) => Valid(Benchmarks.dixonPrice(a.convertSized2And)) }
  //val katsuura = Unconstrained { (a: List[Double]) => Valid(Benchmarks.katsuura(a.convertNel)) }
  val quadric = Unconstrained { (a: List[Double]) => Valid(Benchmarks.quadric(a.convertNel)) }
  val zakharov = Unconstrained { (a: List[Double]) => Valid(Benchmarks.zakharov(a.convertNel)) }
  val brown = Unconstrained { (a: List[Double]) => Valid(Benchmarks.brown(a.convertSized2And)) }

  // multi-modal / non-separable
  val ackley    = Unconstrained { (a: List[Double]) => Valid(Benchmarks.ackley(a.convertNel)) }
  val eggHolder = Unconstrained { (a: List[Double]) => Valid(Benchmarks.eggHolder(a.convertSized2And)) }
  val exponential1 = Unconstrained { (a: List[Double]) => Valid(Benchmarks.exponential1(a.convertNel)) }
//  val norwegian = Unconstrained { (a: List[Double]) => Valid(Benchmarks.norwegian(a.convertNel)) }
  val salomon = Unconstrained { (a: List[Double]) => Valid(Benchmarks.salomon(a.convertNel)) }
  val mishra1 = Unconstrained { (a: List[Double]) => Valid(Benchmarks.mishra1(a.convertNel)) }

  // multi-modal / separable
  val alpine1 = Unconstrained { (a: List[Double]) => Valid(Benchmarks.alpine1(a.convertNel)) }
  val michalewicz = Unconstrained { (a: List[Double]) => Valid(Benchmarks.michalewicz(10)(a.convertNel)) }
  val step1 = Unconstrained { (a: List[Double]) => Valid(Benchmarks.step1(a.convertNel)) }
  //val vincent = Unconstrained { (a: List[Double]) => Valid(Benchmarks.vincent(a.convertNel)) }
  val weierstrass = Unconstrained { (a: List[Double]) => Valid(Benchmarks.weierstrass(a.convertNel)) }
  val cosineMixture = Unconstrained { (a: List[Double]) => Valid(Benchmarks.cosineMixture(a.convertNel)) }

  def problemsClasses(dim: Int) = Map(
    // "um-s" -> List(
    //   ProblemDef("absolute", Problems.absolute, -100.0, 100.0, dim),
    //   ProblemDef("differentPowers", Problems.differentPowers, -100.0, 100.0, dim),
    //   ProblemDef("hyperEllipsoid", Problems.hyperEllipsoid, -10.0, 10.0, dim),
    //   ProblemDef("powellSum", Problems.powellSum, -1.0, 1.0, dim),
    //   ProblemDef("spherical", Problems.spherical, -100.0, 100.0, dim)
    // ),
    // "um-ns" -> List(
    //   ProblemDef("brent", Problems.brent, -10.0, 10.0, dim),
    //   ProblemDef("dixonPrice", Problems.dixonPrice, -10.0, 10.0, dim),
    //   ProblemDef("brown", Problems.brown, -1.0, 1.0, dim),
    //   ProblemDef("quadric", Problems.quadric, -100.0, 100.0, dim),
    //   ProblemDef("zakharov", Problems.zakharov, -5.0, 10.0, dim)
    // ),
    // "mm-s" -> List(
    //   ProblemDef("alpine1", Problems.alpine1, -10.0, 10.0, dim),
    //   ProblemDef("michalewicz", Problems.michalewicz, 0.0, Math.PI, dim),
    //   ProblemDef("step1", Problems.step1, -100.0, 100.0, dim),
    //   ProblemDef("cosineMixture", Problems.cosineMixture, -1.0, 1.0, dim),
    //   ProblemDef("weierstrass", Problems.weierstrass, -0.5, 0.5, dim)
    // ),
    "mm-ns" -> List(
      // ProblemDef("ackley", Problems.ackley, -32.768, 32.768, dim),
      // ProblemDef("eggHolder", Problems.eggHolder, -512.0, 512.0, dim),
      // ProblemDef("exponential1", Problems.exponential1, -1.0, 1.0, dim),
      // ProblemDef("salomon", Problems.salomon, -100.0, 100.0, dim)
        ProblemDef("mishra1", Problems.mishra1, 0.0, 1.0, dim)
    )
  )


//  def hyperEllipsoid = Unconstrained { (a: List[Double]) => Valid(Benchmarks.hyperEllipsoid(a.convertNel)) }
//  def schwefel221 = Unconstrained { (a: List[Double]) => Valid(Benchmarks.schwefel221(a.convertSized1And)) }
//  def adjiman   = Unconstrained { (a: List[Double]) => Valid(Benchmarks.adjiman(a.convertSized2)) }

  // Not sure where to put these yet....

  /* G13 Problems. Runarrson */

  // This needs to be something that is "sized"
  /*val g1 = Problem.violations(
    Problem.static((a: List[Double]) => {
      val x = a.take(4).sum * 5.0
      val y = a.take(4).map(x => x*x).sum * 5.0
      val z = a.drop(4).sum
      Valid(x - y - z)
    }),
    List(
      (a: List[Double]) => Violation.bool( 2*a(0) + 2*a(1) + a(9) + a(10) - 10 <= 0),
      (a: List[Double]) => Violation.bool( 2*a(0) + 2*a(2) + a(9) + a(11) - 10 <= 0),
      (a: List[Double]) => Violation.bool( 2*a(0) + 2*a(2) + a(10) + a(11) - 10 <= 0),
      (a: List[Double]) => Violation.bool(-8*a(0) + a(9) <= 0),
      (a: List[Double]) => Violation.bool(-8*a(1) + a(10) <= 0),
      (a: List[Double]) => Violation.bool(-8*a(2) + a(11) <= 0),
      (a: List[Double]) => Violation.bool(-2*a(3) - a(4) + a(9) <= 0),
      (a: List[Double]) => Violation.bool(-2*a(5) - a(6) + a(10) <= 0),
      (a: List[Double]) => Violation.bool(-2*a(7) - a(8) + a(11) <= 0)
    )
  )*/

  case class Peak(pos: List[Double], width: Double, height: Double, movementDirection: List[Double], shiftVector: List[Double])
  case class PeakState(peaks: List[Peak], interval: NonEmptyList[Interval[Double]],
    frequency: Int = 10,
    widthSeverity: Double = 0.01, heightSeverity: Double = 7.0,
    shiftSeverity: Double = 1.0, lambda: Double = 0.75,
    minHeight: Double = 30.0, maxHeight: Double = 70.0, minWidth: Double = 1.0, maxWidth: Double = 12.0)

  import scalaz._

  def initPeaks[F[_]:SolutionRep:Foldable](n: Int, interval: NonEmptyList[Interval[Double]],
    minHeight: Double, maxHeight: Double, minWidth: Double, maxWidth: Double
  ): RVar[(PeakState, Eval[F,Double])] = {
    val t = List.fill(interval.size)(1.0)
    val peaks = (1 to n).toList.traverse(_ => {
      val position = interval.list.traverse(x => Dist.uniform(x.lower.value, x.upper.value))
      val height = Dist.uniform(minHeight, maxHeight)
      val width = Dist.uniform(minWidth, maxWidth)

      (position |@| width |@| height) { Peak(_, _, _, t, t) }
    })

    peaks.map(p => (PeakState(p, interval, 10, 0.01, 7.0, 1.0, 0.75, minHeight, maxHeight, minWidth, maxWidth), movingPeaksEval(p)))
  }

  def modifyPeaks[F[_]:SolutionRep:Foldable]: StateT[RVar, PeakState, Eval[F,Double]] =
    StateT {
      ps => {
        val newPeaks = ps.peaks.traverse(peak => {
          val heightOffset: RVar[Double] = Dist.stdNormal.map(x => {
            val offset = x * ps.heightSeverity
            if (peak.height + offset > ps.maxHeight || peak.height - offset < ps.minHeight)
              peak.height - offset
            else
              peak.height + offset
          })
          val widthOffset  = Dist.stdNormal.map(x => {
            val offset = x * ps.widthSeverity
            if (peak.width + offset > ps.maxWidth || peak.width - offset < ps.minWidth)
              peak.width - offset
            else
              peak.width + offset
          })

          val shift = peak.pos + ((peak.shiftVector, peak.movementDirection).zipped map { _ * _ })
          val newDirection = (shift, peak.movementDirection, ps.interval.list).zipped.map { case (a,b,c) => if (a > c.upper.value || a < c.lower.value) b * -1.0 else b }
          val newShift = (shift, peak.shiftVector, ps.interval.list).zipped.map { case (a,b,c) => if (a > c.upper.value || a < c.lower.value) b * -1.0 else b }
          val newPos = peak.pos + newShift

          (widthOffset |@| heightOffset) { Peak(newPos, _, _, newDirection, newShift) }
        })

        newPeaks.map(np => (ps.copy(peaks = np), movingPeaksEval(np)))
      }
    }

  def movingPeaksEval[F[_]:SolutionRep](peaks: List[Peak])(implicit F: Foldable[F]) =
    new Unconstrained[F,Double]((a: F[Double]) => {
      import scalaz.syntax.foldable._
      import scalaz.std.anyVal._

      val r = peaks.map(x => {
        val h = (a.toList - x.pos).foldLeft(0.0)((acc,y) => acc + y*y)
        val w = 1 + (h * x.width)
        x.height / w
      })
      Valid(r.maximum.getOrElse(-1.0))
    })
}
