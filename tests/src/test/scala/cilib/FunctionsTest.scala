package cilib

import cilib.Functions._
import cilib.FunctionWrappers._

import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

import spire.math._
import spire.implicits._

object FunctionsTest extends Properties("Functions") {

  val zero = List(0.0, 0.0, 0.0)
  def accurate(v: Double, d: Double, e: Double) = abs(v - d) <= e

  def epsilon(precision: Double) = 1.0 / (10.0 ** precision)
  val epsilon = 1e-15

  def debug(f: Seq[Double] => Option[Double], x: List[Double]) =
    println(f(x) + " " + x)

  def gen(a: Double, b: Double) =
    Gen.containerOf[List, Double](Gen.choose(a, b))
  def genN(n: Int, a: Double, b: Double) =
    Gen.containerOfN[List, Double](n, Gen.choose(a, b))
  def genArbN(n: Int) =
    Gen.containerOfN[List, Double](n, Arbitrary.arbitrary[Double])
  def genConst(a: Double) =
    Gen.containerOf[List, Double](Gen.const(a))

  implicit class OptionDoubleOps(o: Option[Double]) {
    def >=(d: Double) = o.forall(_ >= d)
    def <=(d: Double) = o.forall(_ <= d)
    def ~(d: Double, e: Double) = o.forall(accurate(_, d, e))
  }

  property("absoluteValue") = forAll { (g: List[Double]) =>
    val abs = absoluteValue(g)
    abs == absoluteValue(g.map(_ * -1)) &&
    abs >= 0.0 &&
    abs >= g.sum
  } && {
    absoluteValue(zero) == Some(0.0) &&
    absoluteValue(List(1.0, 2.0, 3.0)) == Some(6.0) &&
    absoluteValue(List(1.0, 2.0, 3.0).map(_ * -1)) == Some(6.0)
  }

  property("ackley") = forAll(gen(-32.768, 32.768)) { g =>
    ackley(g) >= 0.0
  } && ackley(zero).forall(_ < epsilon)


  property("adjiman") = forAll(genN(2, -5.0, 5.0)) { g =>
    adjiman(g) >= -5.02181
  } && adjiman(List(2, 0.10578)) == Some(-2.0218067833370204)

  property("alpine1") = forAll { (g: List[Double]) =>
    alpine1(g) >= 0.0
  } && alpine1(zero) == Some(0.0)

  property("arithmeticMean") = forAll(gen(0.0, 1.0)) { g =>
    arithmeticMean(g) >= 0.0
  } && arithmeticMean(zero) == Some(0.0)

  property("bartelsConn") = forAll(genN(2, -50.0, 50.0)) { g =>
    bartelsConn(g) >= 1.0
  } && {
    bartelsConn(List(0.0, 0.0)) == Some(1.0) &&
    bartelsConn(zero) == None
  }

  property("beale") = forAll(gen(-4.5, 4.5)) { g =>
    beale(g) >= 0.0
  } && beale(List(3, 0.5)) == Some(0.0)

  property("bohachevsky") = forAll(genN(2, -100.0, 100.0)) { g =>
    bohachevsky1(g) >= 0.0 &&
    bohachevsky2(g) >= 0.0 &&
    bohachevsky3(g) >= 0.0
  } && {
    bohachevsky1(List(0.0, 0.0)) == Some(0.0) &&
    bohachevsky2(List(0.0, 0.0)) == Some(0.0) &&
    bohachevsky3(List(0.0, 0.0)) == Some(0.0) &&
    bohachevsky1(zero) == None &&
    bohachevsky2(zero) == None &&
    bohachevsky3(zero) == None
  }

  property("booth") = forAll(genN(2, -10.0, 10.0)) { g =>
    booth(g) >= 0.0
  } && {
    booth(List(1.0, 3.0)) == Some(0.0)
    booth(zero) == None
  }

  val genBraninRCOS = Gen.containerOfN[List, List[Double]](1, for {
    x1 <- Gen.choose(-5.0, 10.0)
    x2 <- Gen.choose(0.0, 15.0)
  } yield List(x1, x2))

  property("braninRCOS1") = forAll(genBraninRCOS) { g =>
    braninRCOS1(g.flatten) >= 0.3978874 - epsilon
  } && braninRCOS1(List(-pi, 12.275)) ~ (0.3978874, epsilon(5))

  property("brent") = forAll(gen(-10.0, 10.0)) { g =>
    brent(g) >= 0.0
  } && brent(List(-10.0, -10.0, -10.0)) ~ (0, epsilon)

  property("brown") = forAll(gen(-1.0, 1.0)) { g =>
    brown(g) >= 0.0
  } && brown(zero) == Some(0.0)

  val genBukin = Gen.containerOfN[List, List[Double]](1, for {
    a <- Gen.choose(-15.0, -5.0)
    b <- Gen.choose(-3.0, 3.0)
  } yield List(a, b))

  property("bukin") = forAll(genBukin) { g =>
    bukin2(g.flatten) >= 0.0 &&
    bukin2Adapted(g.flatten) >= 0.0 &&
    bukin4(g.flatten) >= 0.0 &&
    bukin6(g.flatten) >= 0.0
  } && {
    bukin2(List(-10.0, 0.0))        == Some(0.0) &&
    bukin2Adapted(List(-10.0, 0.0)) == Some(0.0) &&
    bukin4(List(-10.0, 0.0))        == Some(0.0) &&
    bukin6(List(-10.0, 1.0))        == Some(0.0) &&
    bukin2(zero)                    == None &&
    bukin2Adapted(zero)             == None &&
    bukin4(zero)                    == None &&
    bukin6(zero)                    == None
  }

  property("centralTwoPeakTrap") = forAll(genN(1, 0.0, 20.0)) { g =>
    centralTwoPeakTrap(g) >= -200.0
  } && centralTwoPeakTrap(List(20.0)) == Some(-200.0)

  property("chichinadze") = forAll(genN(2, -30.0, 30.0)) { g =>
    chichinadze(g) >= -43.3159
  } && {
    chichinadze(List(5.90133, 0.5)) ~ (-43.3159, epsilon(4))
  }

  property("chungReynolds") = forAll(gen(-100.0, 100.0)) { g =>
    chungReynolds(g) >= 0.0
  } && chungReynolds(zero) == Some(0.0)

  property("cigar") = forAll { (g: List[Double]) =>
    cigar(g) >= 0.0
  } && cigar(zero) == Some(0.0)

  property("colville") = forAll(genN(4, -10.0, 10.0)) { g =>
    colville(g) >= 0.0
  } && {
    colville(List(1.0, 1.0, 1.0, 1.0)) == Some(0.0) &&
    colville(List(0.0, 0.0, 0.0, 0.0)) == Some(42.0)
  }

  property("cosineMixture") = forAll(gen(-1.0, 1.0)) { g =>
    cosineMixture(g) >= -0.1 * g.length
  } && cosineMixture(zero) ~ (-0.1 * zero.length, epsilon(5))

  property("cross") = forAll(gen(-10.0, 10.0)) { g =>
    crossInTray(g) >= -2.11 &&
    crossLegTable(g) >= -1.0 &&
    crossCrowned(g) >= -0.0001
  } && {
    crossInTray(List(1.349406685353340,1.349406608602084)) ~
      (-2.06261218, epsilon(6)) &&
    crossLegTable(zero) == Some(-1.0) &&
    crossCrowned(zero) == Some(0.0001)
  }

  property("csendes") = forAll(gen(-1.0, 1.0)) { g =>
    csendes(g) >= 0.0
  } //&& csendes(zero) == Some(NaN)

  property("cube") = forAll(genN(2, -10.0, 10.0)) { g =>
    cube(g) >= 0.0
  } && {
    cube(List(1.0, 1.0)) == Some(0.0) &&
    cube(List(-1.0, 1.0)) == Some(404.0)
  }

  property("damavandi") = forAll(genN(2, 0.0, 14.0)) { g =>
    damavandi(g) >= 0.0
  }

  property("deb") = forAll(gen(0.0, 1.0)) { g =>
    deb1(g) >= -1.0 &&
    deb3(g) >= -1.0
  } && deb1(zero) == Some(0.0)

  property("decanomial") = forAll(genN(2, -10.0, 10.0)) { g =>
    decanomial(g) >= 0.0
  } && decanomial(List(2.0, -3.0)) == Some(0.0)

  property("deckkersAarts") = forAll(genN(2, -20.0, 20.0)) { g =>
    deckkersAarts(g) >= -24777.0
  } && {
    deckkersAarts(List(0.0, 15.0))  ~ (-24771.0, epsilon(0)) &&
    deckkersAarts(List(0.0, -15.0)) ~ (-24771.0, epsilon(0))
  }

  property("deVilliersGlasser1") = forAll(genN(4, 1.0, 100.0)) { g =>
    deVilliersGlasser1(g) >= 0.0
  }

  property("deVilliersGlasser2") = forAll(genN(5, 1.0, 6.0)) { g =>
    deVilliersGlasser2(g) >= 0.0
  }

  property("differentPowers") = forAll(gen(-100.0, 100.0)) { g =>
    differentPowers(g) >= 0.0
  } && differentPowers(zero) == Some(0.0)

  property("discus") = forAll(gen(-100.0, 100.0)) { g =>
    discus(g) >= 0.0
  } && {
    discus(zero)           == Some(0.0) &&
    discus(List(1.0))      == Some(1e6) &&
    discus(List(1.0, 1.0)) == Some(1e6 + 1.0)
  }

  property("dixonPrice") = forAll(gen(-10.0, 10.0)) { g =>
    dixonPrice(g) >= 0.0
  } && dixonPrice(List(1.0, 1.0 / sqrt(2))) ~ (0.0, epsilon)

  property("dropWave") = forAll(gen(-5.12, 5.12)) { g =>
    dropWave(g) >= -1.0
  } && dropWave(zero) == Some(-1.0)

  property("easom") = forAll(genN(2, -100.0, 100.0)) { g =>
    easom(g) >= -1.0
  } && easom(List(Math.PI, Math.PI)) == Some(-1.0)

  property("eggCrate") = forAll(gen(-5.0, 5.0)) { g =>
    eggCrate(g) >= 0.0
  } && eggCrate(zero) == Some(0.0)

  property("eggHolder") = forAll(gen(-512.0, 512.0)) { g =>
    eggHolder(g) >= -959.64 * g.length
  } && eggHolder(List(512.0, 404.2319)) ~ (-959.64, epsilon(3))

  property("elliptic") = forAll { (g: Seq[Double]) =>
    elliptic(g) >= 0.0
  } && elliptic(zero) == Some(0.0)

  property("elAttarVidyasagarDutta") = forAll(genN(2, -100.0, 100.0)) { g =>
    elAttarVidyasagarDutta(g) >= 1.712780354
  } && elAttarVidyasagarDutta(
    List(3.40918683, -2.17143304)) ~ (1.712780354, epsilon(9))

  property("exponential1") = forAll(gen(-1.0, 1.0)) { g =>
    exponential1(g) >= -1.0
  } && exponential1(zero) == Some(-1.0)

  property("exponential2") = forAll(gen(0.0, 20.0)) { g =>
    exponential2(g) >= 0.0
  } && exponential2(List(1.0, 10.0)) ~ (0.0, epsilon)

  property("freudensteinRoth") = forAll(genN(2, -10.0, 10.0)) { g =>
    freudensteinRoth(g) >= 0.0
  } && freudensteinRoth(List(5.0, 4.0)) == Some(0.0)

  property("gear") = forAll(genN(4, 12.0, 60.0)) { g =>
    gear(g) >= 2.7 * 10e-12
  } && gear(List(16.0, 19.0, 43.0, 49.0)) ~ (2.7 * 10e-12, epsilon(10))

  property("giunta") = forAll(genN(2, -1.0, 1.0)) { g =>
    giunta(g) >= 0.06447042053690566
  } && giunta(List(0.45834282, 0.45834282)) ~ (0.06447042053690566, epsilon(3))

  property("goldsteinPrice1") = forAll(genN(2, -2.0, 2.0)) { g =>
    goldsteinPrice1(g) >= 3.0
  } && {
    goldsteinPrice1(List(1.2, 0.8)) ~ (840.0, epsilon(12)) &&
    goldsteinPrice1(List(1.8, 0.2)) ~ (84.0, epsilon(12)) &&
    goldsteinPrice1(List(-0.6, -0.4)) == Some(30.0) &&
    goldsteinPrice1(List(0.0, -1.0))  == Some(3.0)
  }

  property("goldsteinPrice2") = forAll(genN(2, -5.0, 5.0)) { g =>
    goldsteinPrice2(g) >= 1.0
  } && {
    goldsteinPrice2(List(3.0, 4.0)) == Some(1.0)
  }

  property("griewank") = forAll { (g: List[Double]) =>
    griewank[Double](g) >= 0.0
  } && griewank[Double](zero) == Some(0.0)

  property("hansen") = forAll(genN(2, -10.0, 10.0)) { g =>
    hansen(g) >= -176.54
  } && {
    hansen(List(-7.58993, -7.708314))  ~ (-176.54, epsilon(2)) &&
    hansen(List(-7.58993, -1.425128))  ~ (-176.54, epsilon(2)) &&
    hansen(List(-7.58993, 4.858057))   ~ (-176.54, epsilon(2)) &&
    hansen(List(-1.306708, -7.708314)) ~ (-176.54, epsilon(2)) &&
    hansen(List(-1.306708, 4.858057))  ~ (-176.54, epsilon(2)) &&
    hansen(List(4.976478, 4.858057))   ~ (-176.54, epsilon(2)) &&
    hansen(List(4.976478, -1.425128))  ~ (-176.54, epsilon(2)) &&
    hansen(List(4.976478, -7.708314))  ~ (-176.54, epsilon(2))
  }

  property("hartman3") = forAll(gen(0.0, 1.0)) { g =>
    hartman3(g) >= -3.862782
  } && hartman3(List(0.1140, 0.556, 0.852)) ~ (-3.862782, epsilon(4))

  property("hartman6") = forAll(gen(-5.0, 5.0)) { g =>
    hartman6(g) >= -3.32236
  } && hartman6(List(0.201690, 0.150011, 0.476874,
    0.275332, 0.311652, 0.657301)) ~ (-3.32236, epsilon(5))

  property("helicalValley") = forAll(gen(-10.0, 10.0)) { g =>
    helicalValley(g) >= 0.0
  } && helicalValley(List(1.0, 0.0, 0.0)) == Some(0.0)

  property("himmelblau") = forAll(genArbN(2)) { g =>
    himmelblau(g) >= 0.0
  } && himmelblau(List(3.0, 2.0)) == Some(0.0)

  val genHolzman = Gen.containerOfN[List, List[Double]](1, for {
    a <- Gen.choose(0.1, 100.0)
    b <- Gen.choose(0.0, 25.6)
    c <- Gen.choose(0.0, 5.0)
  } yield List(a, b))

  property("holzman") = forAll(genHolzman) { g =>
    holzman(g.flatten) >= 0.0
  } && holzman(List(50.0, 25.0, 1.5)) ~ (0.0, epsilon(14))

  property("hosaki") = forAll(genN(2, 0.0, 10.0)) { g =>
    hosaki(g) >= -2.3458
  } && hosaki(List(4.0, 2.0)) ~ (-2.3458, epsilon(4))

  property("hyperEllipsoid") = forAll(gen(-10.0, 10.0)) { g =>
    hyperEllipsoid(g.map(-_)) == hyperEllipsoid(g) &&
    hyperEllipsoid(g) >= 0.0
  } && hyperEllipsoid(zero) == Some(0.0)

  property("hyperEllipsoidRotated") = forAll(gen(-65.536, 65.536)) { g =>
    hyperEllipsoidRotated (g) >= 0.0
  } && hyperEllipsoidRotated(zero) == Some(0.0)

  property("infinity") = forAll(gen(-1.0, 1.0)) { g =>
    infinity(g) >= 0.0
  } && infinity(zero) == None

  property("jennrichSampson") = forAll(genN(2, -1.0, 1.0)) { g =>
    jennrichSampson(g) >= 124.3612
  } && jennrichSampson(List(0.257825, 0.257825)) ~ (124.3612, epsilon(3))

  property("judge") = forAll(genN(2, -10.0, 10.0)) { g =>
    judge(g) >= 16.0817307
  } && judge(List(0.86479, 1.2357)) ~ (16.0817307, epsilon(4))

  property("katsuura") = forAll(gen(0.0, 100.0)) { g =>
    katsuura(g) >= 1.0
  } && katsuura(zero) == Some(1.0)

  property("kowalik") = forAll(genN(4, -5.0, 5.0)) { g =>
    kowalik(g) >= 0.0003074861
  } && kowalik(List(0.192833, 0.190836, 0.123117, 0.135766)) ~
    (0.0003074861, epsilon(8))

  property("leon") = forAll(gen(-1.2, 1.2)) { g =>
    leon(g) >= 0.0
  } && leon(List(1.0, 1.0)) == Some(0.0)

  property("levy3") = forAll(gen(-10.0, 10.0)) { g =>
    levy3(g) >= 0.0
  } && forAll(genConst(1.0)) { g =>
    levy3(g) ~ (0.0, epsilon)
  }

  property("levy5-13") = forAll(genN(2, -10.0, 10.0)) { g =>
     levy5(g) >= -176.1375 &&
     levy13(g) >= 0.0
  } && {
     levy5(List(-1.3068, -1.4248)) ~ (-176.1375, epsilon(4)) &&
     levy13(List(1.0, 1.0)) ~ (0.0, epsilon)
  }

  property("levyMontalvo2") = forAll(gen(-5.0, 5.0)) { g =>
    levyMontalvo2(g) >= 0.0
  } && forAll(genConst(1.0)) { g =>
    levyMontalvo2(g) ~ (0.0, epsilon)
  }

  property("matyas") = forAll(genN(2, -10.0, 10.0)) { g =>
    matyas(g) >= 0.0
  } && matyas(List(0.0, 0.0)) == Some(0.0)

  property("maximum") = forAll { (g: List[Double]) =>
    maximum(g) == Some(g.max) &&
    maximum(g).forall(gi => g.exists(_ == gi))
  }

  val genMcCormick = Gen.containerOfN[List, List[Double]](1, for {
    a <- Gen.choose(-1.5, 1.5)
    b <- Gen.choose(-3.0, 4.0)
  } yield List(a, b))

  property("mcCormick") = forAll(genMcCormick) { g =>
    mcCormick(g.flatten) >= -1.9133
  } && mcCormick(List(-0.547, -1.547)) ~ (-1.9133, epsilon(4))

  property("michalewicz") = forAll(gen(0.0, Math.PI)) { g =>
    michalewicz(g) >= -0.966 * g.length
  } && michalewicz(List(2.20, 1.57)) ~ (-1.8013, epsilon(3))

  property("mieleCantrell") = forAll(genN(4, -1.0, 1.0)) { g =>
    mieleCantrell(g) >= 0.0
  } && mieleCantrell(List(0.0, 1.0, 1.0, 1.0)) == Some(0.0)

  property("minimum") = forAll { (g: List[Double]) =>
    g match {
      case List() => minimum(g) == None
      case _      => {
        minimum(g) == Some(g.min) &&
        minimum(g).forall(gi => g.exists(_ == gi))
      }
    }
  }

  property("mishra1-2") = forAll(gen(0.0, 1.0)) { g =>
    mishra1(g) >= 2.0 &&
    mishra2(g) >= 2.0
  } && forAll(genConst(1.0)) { g => g match {
    case List() => {
      mishra1(g) == None &&
      mishra2(g) == None
    }
    case List(_) => mishra2(g) == None
    case _ => {
      mishra1(g) == Some(2.0) &&
      mishra2(g) == Some(2.0)
    }
  }}

  property("mishra5-8-11") = forAll(genN(2, -10.0, 10.0)) { g =>
    mishra5(g)  >= -0.119829 &&
    mishra8(g)  >= 0.0 &&
    mishra11(g) >= 0.0
  } && {
    mishra5(List(-1.98682, -10.0)) ~ (-0.119829, epsilon(5)) &&
    mishra8(List(2.0, -3.0)) == Some(0.0) &&
    mishra11(List(0.0, 0.0)) == Some(0.0)
  }

  property("multiModal") = forAll(gen(-10.0, 10.0)) { g =>
    multiModal(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    multiModal(g) == Some(0.0)
  }

  property("rotatedEllipse") = forAll(gen(-500.0, 500.0)) { g =>
    rotatedEllipse1(g) >= 0.0 &&
    rotatedEllipse2(g) >= 0.0
  } && {
    rotatedEllipse1(zero) == Some(0.0) &&
    rotatedEllipse2(zero) == Some(0.0)
  }

  property("sixHumpCamelback") = forAll(genN(2, -5.0, 5.0)) { g =>
    sixHumpCamelback(zero) >= -1.0316285
  } && {
    sixHumpCamelback(List(-0.08983, 0.7126)) ~ (-1.0316285, epsilon(5)) &&
    sixHumpCamelback(List(0.08983, -0.7126)) ~ (-1.0316285, epsilon(5))
  }

  property("spherical") = forAll { (g: List[Double]) =>
    spherical(g) == spherical(g.map(_ * -1)) &&
    spherical(g) >= 0.0
  }

  property("step") = forAll { (g: List[Double]) =>
    step[Double](g) >= 0.0
  } && {
    step[Double](zero) == Some(0.75) &&
    step[Double](List(1.3, 2.5, 3.7)) == Some(2.25 + 6.25 + 12.25)
  }

  property("schwefel") = forAll(gen(-500.0, 500.0)) { g =>
    schwefel(g) >= 0
  }

  property("schwefel12") = forAll(gen(-500.0, 500.0)) { g =>
    schwefel12(g) >= 0
  }

  property("schwefel221") = forAll { (g: List[Double]) =>
    schwefel221(g) >= 0
  }

  property("schwefel222") = forAll(gen(-500.0, 500.0)) { g =>
    schwefel222(g) >= 0
  }

  property("shubert") = forAll(genN(2, -5.12, 5.12)) { g =>
    shubert(g) >= -186.7309
  }

  property("threeHumpCamelback") = forAll(genN(2, -5.0, 5.0)) { g =>
    threeHumpCamelback(g) >= 0.0
  } && threeHumpCamelback(List(0.0, 0.0)) == Some(0.0)

  property("vincent") = forAll(gen(0.25, 10.0)) { g =>
    vincent(g) >= -g.length + 0.0 &&
    vincent(g) <= g.length + 0.0
  }

  property("zakharov") = forAll(gen(-5.00, 10.0)) { g =>
    zakharov(g) >= 0.0
  } && zakharov(zero) == Some(0.0)

  property("zettle") = forAll(genArbN(2)) { g =>
    zettle(g) >= -0.0037912371501199
  } && zettle(List(-0.0299, 0.0)) == Some(-0.0037912371501199)

  property("wood") = forAll(genN(4, -100.0, 100.0)) { g =>
    wood(g) >= 0.0
  } && wood(List(1.0, 1.0, 1.0, 1.0)) == Some(0.0)
}

object FunctionWrappersTest extends Properties("FunctionWrappers") {

  val y = List(1.0, 2.0, 3.0)
  val zero = List(0.0, 0.0, 0.0)

  val shift = shifted[Double]((spherical _), 10.0, 10.0)

  property("shifted") = forAll { (g: List[Double]) =>
    shift(g).forall(sh => spherical(g).forall(sp => sh >= sp - 10.0))
  } && {
    shift(zero) == Some(100 * 3 - 10.0) &&
    shift(y) == Some(81.0 + 64.0 + 49.0 - 10.0)
  }

}
