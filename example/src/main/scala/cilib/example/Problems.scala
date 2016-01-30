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
  // val absolute  = Unconstrained { (a: List[Double]) => Valid(Benchmarks.absoluteValue(a.convertNel)) }
  // val differentPowers = Unconstrained { (a: List[Double]) => Valid(Benchmarks.differentPowers(a.convertSized2And)) }
  // val hyperEllipsoid = Unconstrained { (a: List[Double]) => Valid(Benchmarks.hyperEllipsoid(a.convertNel)) }
  // val powellSum = Unconstrained { (a: List[Double]) => Valid(Benchmarks.powellSum(a.convertNel)) }
  // val spherical = Unconstrained { (a: List[Double]) => Valid(Benchmarks.spherical(a.convertNel)) }

  // uni-modal / non-separable
  // val brent = Unconstrained { (a: List[Double]) => Valid(Benchmarks.brent(a.convertNel)) }
  // val dixonPrice = Unconstrained { (a: List[Double]) => Valid(Benchmarks.dixonPrice(a.convertSized2And)) }
  // val katsuura = Unconstrained { (a: List[Double]) => Valid(Benchmarks.katsuura(a.convertNel)) }
  // val quadric = Unconstrained { (a: List[Double]) => Valid(Benchmarks.quadric(a.convertNel)) }
  // val zakharov = Unconstrained { (a: List[Double]) => Valid(Benchmarks.zakharov(a.convertNel)) }
  // val brown = Unconstrained { (a: List[Double]) => Valid(Benchmarks.brown(a.convertSized2And)) }

  // multi-modal / non-separable
  // val ackley    = Unconstrained { (a: List[Double]) => Valid(Benchmarks.ackley(a.convertNel)) }
  // val eggHolder = Unconstrained { (a: List[Double]) => Valid(Benchmarks.eggHolder(a.convertSized2And)) }
  // val exponential1 = Unconstrained { (a: List[Double]) => Valid(Benchmarks.exponential1(a.convertNel)) }
  // val norwegian = Unconstrained { (a: List[Double]) => Valid(Benchmarks.norwegian(a.convertNel)) }
  // val salomon = Unconstrained { (a: List[Double]) => Valid(Benchmarks.salomon(a.convertNel)) }
  // val mishra1 = Unconstrained { (a: List[Double]) => Valid(Benchmarks.mishra1(a.convertNel)) }
  // val pathological = Unconstrained { (a: List[Double]) => Valid(Benchmarks.pathological(a.convertSized2And)) }

  // multi-modal / separable
  // val alpine1 = Unconstrained { (a: List[Double]) => Valid(Benchmarks.alpine1(a.convertNel)) }
  // val michalewicz = Unconstrained { (a: List[Double]) => Valid(Benchmarks.michalewicz(10)(a.convertNel)) }
  // val step1 = Unconstrained { (a: List[Double]) => Valid(Benchmarks.step1(a.convertNel)) }
  // val vincent = Unconstrained { (a: List[Double]) => Valid(Benchmarks.vincent(a.convertNel)) }
  // val weierstrass = Unconstrained { (a: List[Double]) => Valid(Benchmarks.weierstrass(a.convertNel)) }
  // val cosineMixture = Unconstrained { (a: List[Double]) => Valid(Benchmarks.cosineMixture(a.convertNel)) }

  val sphere = Unconstrained { (a: List[Double]) => Valid(Benchmarks.spherical(a.convertNel)) }

  import cilib.benchmarks.Benchmarks._

  def problemsClasses(dim: Int) = Map(
    "um-s" -> List(
      ProblemDef("absolute", problemNel(absoluteValue), -100.0, 100.0, dim),
      ProblemDef("differentPowers", problem2And(differentPowers), -100.0, 100.0, dim),
      ProblemDef("hyperEllipsoid", problemNel(hyperEllipsoid), -10.0, 10.0, dim),
      ProblemDef("powellSum", problemNel(powellSum), -1.0, 1.0, dim),
      ProblemDef("spherical", problemNel(spherical), -100.0, 100.0, dim)
    ),
    "um-ns" -> List(
      ProblemDef("brent", problemNel(brent), -10.0, 10.0, dim),
      ProblemDef("dixonPrice", problem2And(dixonPrice), -10.0, 10.0, dim),
      ProblemDef("brown", problem2And(brown), -1.0, 1.0, dim),
      ProblemDef("quadric", problemNel(quadric), -100.0, 100.0, dim),
      ProblemDef("zakharov", problemNel(zakharov), -5.0, 10.0, dim)
    ),
    "mm-s" -> List(
      ProblemDef("alpine1", problemNel(alpine1), -10.0, 10.0, dim),
      ProblemDef("michalewicz", problemNel(michalewicz(10)), 0.0, Math.PI, dim),
      ProblemDef("step1", problemNel(step1), -100.0, 100.0, dim),
      ProblemDef("cosineMixture", problemNel(cosineMixture), -1.0, 1.0, dim),
      ProblemDef("weierstrass", problemNel(weierstrass), -0.5, 0.5, dim)
    ),
    "mm-ns" -> List(
      ProblemDef("ackley", problemNel(ackley), -32.768, 32.768, dim),
      ProblemDef("eggHolder", problem2And(eggHolder), -512.0, 512.0, dim),
      ProblemDef("exponential1", problemNel(exponential1), -1.0, 1.0, dim),
      ProblemDef("salomon", problemNel(salomon), -100.0, 100.0, dim),
      ProblemDef("pathological", problem2And(pathological), -100.0, 100.0, dim)
    )
  )

  def problemNel(f: NonEmptyList[Double] => Double) = Unconstrained { (a: List[Double]) => Valid(f(a.convertNel)) }
  def problem1(f: Sized1[Double] => Double) = Unconstrained { (a: List[Double]) => Valid(f(a.convertSized1)) }
  def problem1And(f: Sized1And[List,Double] => Double) = Unconstrained { (a: List[Double]) => Valid(f(a.convertSized1And)) }
  def problem2(f: Sized2[Double] => Double) = Unconstrained { (a: List[Double]) => Valid(f(a.convertSized2)) }
  def problem2And(f: Sized2And[List,Double] => Double) = Unconstrained { (a: List[Double]) => Valid(f(a.convertSized2And)) }
  def problem3(f: Sized3[Double] => Double) = Unconstrained { (a: List[Double]) => Valid(f(a.convertSized3)) }
  def problem4(f: Sized4[Double] => Double) = Unconstrained { (a: List[Double]) => Valid(f(a.convertSized4)) }
  def problem5(f: Sized5[Double] => Double) = Unconstrained { (a: List[Double]) => Valid(f(a.convertSized5)) }
  def problem6(f: Sized6[Double] => Double) = Unconstrained { (a: List[Double]) => Valid(f(a.convertSized6)) }

  def benchmarkSet(dim: Int) = List(
    ProblemDef("absolute", problemNel(absoluteValue), -100.0, 100.0, dim),
    ProblemDef("ackley", problemNel(ackley), -32.768, 32.768, dim),
    ProblemDef("adjiman", problem2(adjiman), -5.0, 5.0, 2),
    ProblemDef("alpine1", problemNel(alpine1), -10.0, 10.0, dim),
    ProblemDef("alpine2", problemNel(alpine2), 0.0, 10.0, dim),
    ProblemDef("arithmeticMean", problemNel(arithmeticMean), 0.0, 1.0, dim),
    ProblemDef("bartelsConn", problem2(bartelsConn), -50.0, 50.0, 2),
    ProblemDef("beale", problem2(beale), -4.5, 4.5, 2),
    ProblemDef("biggsEXP2", problem2(biggsEXP2), 0.0, 20.0, 2),
    ProblemDef("biggsEXP3", problem2(biggsEXP2), 0.0, 20.0, 2),
    ProblemDef("biggsEXP4", problem2(biggsEXP2), 0.0, 20.0, 2),
    ProblemDef("biggsEXP5", problem2(biggsEXP2), 0.0, 20.0, 2),
    ProblemDef("biggsEXP6", problem2(biggsEXP2), -20.0, 20.0, 2),
    ProblemDef("bird", problem2(bird), -2.0 * pi, 2.0 * pi, 2),
    ProblemDef("bohachevsky1", problem2(bohachevsky1), -100.0, 100.0, 2),
    ProblemDef("bohachevsky2", problem2(bohachevsky2), -100.0, 100.0, 2),
    ProblemDef("bohachevsky3", problem2(bohachevsky3), -100.0, 100.0, 2),
    ProblemDef("booth", problem2(booth), -10.0, 10.0, 2),
    ProblemDef("braninRCOS2", problem2(braninRCOS2), -5.0, 15.0, 2),
    ProblemDef("brent", problemNel(brent), -10.0, 10.0, dim),
    ProblemDef("brown", problem2And(brown), -1.0, 1.0, dim),
    ProblemDef("carromTable", problem2(carromTable), -10.0, 10.0, 2),
    ProblemDef("centralTwoPeakTrap", problem1(centralTwoPeakTrap), 0.0, 20.0, 1),
    ProblemDef("chichinadze", problem2(chichinadze), -30.0, 30.0, 2),
    ProblemDef("chungReynolds", problemNel(chungReynolds), -100.0, 100.0, dim),
    ProblemDef("cigar", problem2And(cigar(10e6)), -100.0, 100.0, dim),
    ProblemDef("colville", problem4(colville), -10.0, 10.0, 4),
    ProblemDef("corana", problem4(corana(0.05)), -5.0, 5.0, 4),
    ProblemDef("cosineMixture", problemNel(cosineMixture), -1.0, 1.0, dim),
    ProblemDef("crossInTray", problemNel(crossInTray), -10.0, 10.0, dim),
    ProblemDef("crossLegTable", problemNel(crossLegTable), -10.0, 10.0, dim),
    ProblemDef("crossCrowned", problemNel(crossCrowned), -10.0, 10.0, dim),
    ProblemDef("cube", problem2(cube), -10.0, 10.0, 2),
    ProblemDef("deb1", problemNel(deb1), 0.0, 1.0, dim),
    ProblemDef("deb2", problemNel(deb2), 0.0, 1.0, dim),
    ProblemDef("decanomial", problem2(decanomial), -10.0, 10.0, 2),
    ProblemDef("deckkersAarts", problem2(deckkersAarts), -20.0, 20.0, 2),
    ProblemDef("deflectedCorrugatedSpring", problemNel(deflectedCorrugatedSpring(5)), 0.0, 10.0, dim),
    ProblemDef("deVilliersGlasser1", problem4(deVilliersGlasser1), 1.0, 100.0, 4),
    ProblemDef("deVilliersGlasser2", problem5(deVilliersGlasser2), 1.0, 6.0, 5),
    ProblemDef("differentPowers", problem2And(differentPowers), -100.0, 100.0, dim),
    ProblemDef("discus", problem1And(discus), -100.0, 100.0, dim),
    ProblemDef("dixonPrice", problem2And(dixonPrice), -10.0, 10.0, dim),
    ProblemDef("dolan", problem5(dolan), -100.0, 100.0, 5),
    ProblemDef("dropWave", problemNel(dropWave), -5.12, 5.12, dim),
    ProblemDef("easom", problem2(easom), -100.0, 100.0, 2),
    ProblemDef("eggCrate", problemNel(eggCrate), -5.0, 5.0, dim),
    ProblemDef("eggHolder", problem2And(eggHolder), -512.0, 512.0, dim),
    ProblemDef("elAttarVidyasagarDutta", problem2(elAttarVidyasagarDutta), -100.0, 100.0, 2),
    ProblemDef("elliptic", problem2And(elliptic), -100.0, 100.0, dim),
    ProblemDef("exponential1", problemNel(exponential1), -1.0, 1.0, dim),
    ProblemDef("exponential2", problem2(exponential2), 0.0, 20.0, 2),
    ProblemDef("freudensteinRoth", problem2(freudensteinRoth), -10.0, 10.0, 2),
    ProblemDef("gear", problem4(gear), 12.0, 60.0, 4),
    ProblemDef("giunta", problem2(giunta), -1.0, 1.0, 2),
    ProblemDef("goldsteinPrice1", problem2(goldsteinPrice1), -2.0, 2.0, 2),
    ProblemDef("goldsteinPrice2", problem2(goldsteinPrice2), -5.0, 5.0, 2),
    ProblemDef("griewank", problemNel(griewank), -600.0, 600.0, dim),
    ProblemDef("hansen", problem2(hansen), -10.0, 10.0, 2),
    ProblemDef("hartman3", problem3(hartman3), 0.0, 1.0, 3),
    ProblemDef("hartman6", problem6(hartman6), -5.0, 5.0, 6),
    ProblemDef("helicalValley", problem3(helicalValley), -10.0, 10.0, 3),
    ProblemDef("himmelblau", problem2(himmelblau), -6.0, 6.0, 2),
    ProblemDef("hosaki", problem2(hosaki), 0.0, 10.0, 2),
    ProblemDef("hyperEllipsoid", problemNel(hyperEllipsoid), -10.0, 10.0, dim),
    ProblemDef("hyperEllipsoidRotated", problemNel(hyperEllipsoidRotated), -65.536, 65.536, dim),
    ProblemDef("jennrichSampson", problem2(jennrichSampson), -1.0, 1.0, 2),
    ProblemDef("judge", problem2(judge), -10.0, 10.0, 2),
    ProblemDef("katsuura", problemNel(katsuura), 0.0, 100.0, dim),
    ProblemDef("keane", problem2(keane), 0.0, 10.0, 2),
    ProblemDef("kowalik", problem4(kowalik), -5.0, 5.0, 4),
    ProblemDef("langermann", problem2(langermann), 0.0, 10.0, 2),
    ProblemDef("leon", problem2(leon), -1.2, 1.2, 2),
    ProblemDef("levy3", problem2And(levy3), -10.0, 10.0, dim),
    ProblemDef("levy5", problem2(levy5), -10.0, 10.0, dim),
    ProblemDef("levy13", problem2(levy13), -10.0, 10.0, dim),
    ProblemDef("levyMontalvo2", problem2And(levyMontalvo2), -5.0, 5.0, dim),
    ProblemDef("matyas", problem2(matyas), -10.0, 10.0, 2),
    ProblemDef("maximum", problemNel(maximum), -1000.0, 1000.0, dim),
    ProblemDef("michalewicz", problemNel(michalewicz(10)), 0.0, pi, dim),
    ProblemDef("mieleCantrell", problem4(mieleCantrell), -1.0, 1.0, 4),
    ProblemDef("minimum", problemNel(minimum), -1000.0, 1000.0, dim),
    ProblemDef("mishra1", problemNel(mishra1), 0.0, 1.0, dim),
    ProblemDef("mishra2", problem2And(mishra2), 0.0, 1.0, dim),
    ProblemDef("mishra3", problem2(mishra3), -10.0, 10.0, 2),
    ProblemDef("mishra4", problem2(mishra4), -10.0, 10.0, 2),
    ProblemDef("mishra5", problem2(mishra5), -10.0, 10.0, 2),
    ProblemDef("mishra6", problem2(mishra6), -10.0, 10.0, 2),
    ProblemDef("mishra8", problem2(mishra8), -10.0, 10.0, 2),
    ProblemDef("mishra10", problem2(mishra10), -10.0, 10.0, 2),
    ProblemDef("mishra7", problemNel(mishra7), -10.0, 10.0, dim),
    ProblemDef("mishra11", problemNel(mishra11), -10.0, 10.0, dim),
    ProblemDef("mishra9", problem3(mishra9), -10.0, 10.0, 3),
    ProblemDef("multiModal", problemNel(multiModal), -10.0, 10.0, dim),
    ProblemDef("needleEye", problemNel(needleEye(0.0001)), -10.0, 10.0, dim),
    ProblemDef("newFunction1", problem2(newFunction1), -10.0, 10.0, 2),
    ProblemDef("newFunction2", problem2(newFunction2), -10.0, 10.0, 2),
    ProblemDef("norwegian", problemNel(norwegian), -1.1, 1.1, dim),
    ProblemDef("parsopoulus", problem2(parsopoulus), -5.0, 5.0, 2),
    ProblemDef("pathological", problem2And(pathological), -100.0, 100.0, dim),
    ProblemDef("penalty1", problem2And(penalty1), -50.0, 50.0, dim),
    ProblemDef("penalty2", problem2And(penalty2), -50.0, 50.0, dim),
    ProblemDef("penHolder", problem2(penHolder), -11.0, 11.0, 2),
    ProblemDef("periodic", problemNel(periodic), -10.0, 10.0, dim),
    ProblemDef("pinter", problem2And(pinter), -10.0, 10.0, dim),
    ProblemDef("plateau", problemNel(plateau),-5.12, 5.12, dim),
    ProblemDef("powell", problem4(powell), -4.0, 5.0, 4),
    ProblemDef("powellSum", problemNel(powellSum), -1.0, 1.0, dim),
    ProblemDef("powerSum", problem4(powerSum), 0.0, 4.0, 4),
    ProblemDef("price1", problemNel(price1), -500.0, 500.0, dim),
    ProblemDef("price2", problemNel(price2), -10.0, 10.0, dim),
    ProblemDef("price3", problem2(price3), -50.0, 50.0, 2),
    ProblemDef("price4", problem2(price4), -50.0, 50.0, 2),
    ProblemDef("qing", problemNel(qing), -500.0, 500.0, dim),
    ProblemDef("quadratic", problem2(quadratic), -10.0, 10.0, 2),
    ProblemDef("quadric", problemNel(quadric), -100.0, 100.0, dim),
    ProblemDef("quintic", problemNel(quintic), -10.0, 10.0, dim),
    ProblemDef("rastrigin", problemNel(rastrigin), -5.12, 5.12, dim),
    ProblemDef("ripple1", problemNel(ripple1), 0.0, 1.0, dim),
    ProblemDef("ripple2", problemNel(ripple2), 0.0, 1.0, dim),
    ProblemDef("rosenbrock", problem2And(rosenbrock), -30.0, 30.0, dim),
    ProblemDef("rotatedEllipse1", problem2And(rotatedEllipse1), -500.0, 500.0, dim),
    ProblemDef("rotatedEllipse2", problem2And(rotatedEllipse2), -500.0, 500.0, dim),
    ProblemDef("salomon", problemNel(salomon), -100.0, 100.0, dim),
    ProblemDef("sargan", problemNel(sargan), -100.0, 100.0, dim),
    ProblemDef("schaffer1", problem2And(schaffer1), -100.0, 100.0, dim),
    ProblemDef("schaffer2", problem2And(schaffer2), -100.0, 100.0, dim),
    ProblemDef("schaffer3", problem2And(schaffer3), -100.0, 100.0, dim),
    ProblemDef("schaffer4", problem2And(schaffer4), -100.0, 100.0, dim),
    ProblemDef("schumerSteiglitz", problemNel(schumerSteiglitz), -100.0, 100.0, dim),
    ProblemDef("schwefel1", problemNel(schwefel1), -100.0, 100.0, dim),
    ProblemDef("schwefel12", problemNel(schwefel12), -500.0, 500.0, dim),
    ProblemDef("schwefel220", problemNel(schwefel220), -100.0, 100.0, dim),
    ProblemDef("schwefel221", problem1And(schwefel221), -500.0, 500.0, dim),
    ProblemDef("schwefel222", problemNel(schwefel222), -500.0, 500.0, dim),
    ProblemDef("schwefel223", problemNel(schwefel223), -10.0, 10.0, dim),
    ProblemDef("schwefel225", problem1And(schwefel225), -10.0, 10.0, dim),
    ProblemDef("schwefel226", problemNel(schwefel226), -500.0, 500.0, dim),
    ProblemDef("schwefel236", problem2(schwefel236), 0.0, 500.0, 2),
    ProblemDef("schwefel24", problem1And(schwefel24), 0.0, 10.0, dim),
    ProblemDef("schwefel26", problem2(schwefel26), -100.0, 100.0, 2),
    ProblemDef("shekel5", problem4(shekel5), 0.0, 10.0, 4),
    ProblemDef("shekel7", problem4(shekel7), 0.0, 10.0, 4),
    ProblemDef("shekel10", problem4(shekel10), 0.0, 10.0, 4),
    ProblemDef("shubert1", problem2(shubert1), -10.0, 10.0, 2),
    ProblemDef("shubert3", problem2(shubert3), -10.0, 10.0, 2),
    ProblemDef("shubert4", problem2(shubert4), -10.0, 10.0, 2),
    ProblemDef("sineEnvelope", problem2(sineEnvelope), -100.0, 100.0, 2),
    ProblemDef("sixHumpCamelback", problem2(sixHumpCamelback), -5.0, 5.0, 2),
    ProblemDef("spherical", problemNel(spherical), -100.0, 100.0, dim),
    ProblemDef("step1", problemNel(step1), -100.0, 100.0, dim),
    ProblemDef("step2", problemNel(step2), -100.0, 100.0, dim),
    ProblemDef("step3", problemNel(step3), -100.0, 100.0, dim),
    ProblemDef("stretchedVSineWave", problem2And(stretchedVSineWave), -10.0, 10.0, dim),
    ProblemDef("styblinksiTang", problemNel(styblinksiTang), -5.0, 5.0, dim),
    ProblemDef("sumSquares", problemNel(sumSquares), -10.0, 10.0, dim),
    ProblemDef("sumDifferentPowers", problemNel(sumDifferentPowers), -1.0, 1.0, dim),
    ProblemDef("threeHumpCamelback", problem2(threeHumpCamelback), -5.0, 5.0, 2),
    ProblemDef("trecanni", problem2(trecanni), -5.0, 5.0, 2),
    ProblemDef("trefethen", problem2(trefethen), -10.0, 10.0, 2),
    ProblemDef("trid", problem2And(trid), -20.0, 20.0, dim),
    ProblemDef("trigonometric1", problemNel(trigonometric1), 0.0, pi, dim),
    ProblemDef("trigonometric2", problemNel(trigonometric2), -500.0, 500.0, dim),
    ProblemDef("tripod", problem2(tripod), -100.0, 100.0, 2),
    ProblemDef("ursem4", problem2(ursem4), -2.0, 2.0, 2),
    ProblemDef("venterSobiezcczanskiSobieski", problem2(venterSobiezcczanskiSobieski), -50.0, 50.0, 2),
    ProblemDef("vincent", problemNel(vincent), 0.25, 10.0, dim),
    ProblemDef("watson", problem6(watson), -5.0, 5.0, 6),
    ProblemDef("wayburnSeader1", problem2(wayburnSeader1), -5.0, 5.0, 2),
    ProblemDef("wayburnSeader2", problem2(wayburnSeader2), -500.0, 500.0, 2),
    ProblemDef("wayburnSeader3", problem2(wayburnSeader3), -500.0, 500.0, 2),
    ProblemDef("wavy", problemNel(wavy(10)), -pi, pi, dim),
    ProblemDef("weierstrass", problemNel(weierstrass), -0.5, 0.5, dim),
    ProblemDef("whitley", problemNel(whitley), -10.24, 10.24, dim),
    ProblemDef("wolfe", problem3(wolfe), 0.0, 2.0, 3),
    ProblemDef("wood", problem4(wood), -100.0, 100.0, 4),
    ProblemDef("xinSheYang2", problemNel(xinSheYang2), -2.0 * pi, 2.0 * pi, dim),
    ProblemDef("xinSheYang3", problemNel(xinSheYang3(5.0)), -20.0, 20.0, dim),
    ProblemDef("xinSheYang4", problemNel(xinSheYang4), -10.0, 10.0, dim),
    ProblemDef("yaoLiu4", problemNel(yaoLiu4), -10.0, 10.0, dim),
    ProblemDef("zakharov", problemNel(zakharov), -5.00, 10.0, dim),
    ProblemDef("zeroSum", problemNel(zeroSum), -10.0, 10.0, dim),
    ProblemDef("zettle", problem2(zettle), -1.0, 5.0, 2),
    ProblemDef("zirilli1", problem2(zirilli1), -10.0, 10.0, 2),
    ProblemDef("zirilli2", problem2(zirilli2), -500.0, 500.0, 2)
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
